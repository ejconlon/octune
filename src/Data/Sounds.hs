module Data.Sounds where

import Bowtie (Fix (..), Memo, memoKey, pattern MemoP)
import Control.Concurrent.MVar (MVar, modifyMVar, newMVar, withMVar)
import Control.DeepSeq (NFData (..))
import Control.Exception (Exception)
import Control.Monad (join, unless, void, when, (>=>))
import Control.Monad.Except (Except, runExcept, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Par (IVar, InclusiveRange (..), parFor)
import Control.Monad.Par.Class qualified as Par
import Control.Monad.Primitive (PrimMonad (..), RealWorld)
import Control.Monad.Reader (ReaderT (..), ask, asks, local)
import Control.Monad.State.Strict (State, StateT, execStateT, gets, modify', runState)
import Dahdit.Audio.Binary (QuietLiftedArray (..))
import Dahdit.Audio.Wav.Simple (WAVE (..), WAVEHeader (..), WAVESamples (..), getWAVEFile, putWAVEFile)
import Dahdit.LiftedPrimArray (LiftedPrimArray (..))
import Dahdit.Sizes (ByteCount (..), ElemCount (..))
import Data.Foldable (fold, for_, toList)
import Data.Functor.Foldable (cata)
import Data.Int (Int32)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.PrimPar (ReadPar, runReadPar)
import Data.Primitive.ByteArray (ByteArray (..))
import Data.Primitive.PrimArray
  ( MutablePrimArray
  , PrimArray (..)
  , clonePrimArray
  , copyMutablePrimArray
  , copyPrimArray
  , emptyPrimArray
  , generatePrimArray
  , indexPrimArray
  , mapPrimArray
  , newPrimArray
  , primArrayFromList
  , readPrimArray
  , replicatePrimArray
  , runPrimArray
  , setPrimArray
  , sizeofPrimArray
  , unsafeFreezePrimArray
  , writePrimArray
  )
import Data.Primitive.Types (Prim)
import Data.STRef.Strict (newSTRef, readSTRef, writeSTRef)
import Data.Semigroup (Max (..), Sum (..))
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Topo (TopoErr (..), topoEval, topoSort)
import Data.Typeable (Typeable)
import Paths_octune (getDataFileName)
import System.IO.Unsafe (unsafePerformIO)

-- Prim adapters

zeroPrimArray :: (PrimMonad m, Prim a, Num a) => Int -> m (MutablePrimArray (PrimState m) a)
zeroPrimArray n = do
  marr <- newPrimArray n
  setPrimArray marr 0 n 0
  pure marr

replicateWholePrimArray :: (Prim a) => Int -> PrimArray a -> PrimArray a
replicateWholePrimArray n sarr = runPrimArray $ do
  let srcSize = sizeofPrimArray sarr
      len = n * srcSize
  darr <- newPrimArray len
  for_ [0 .. n - 1] $ \i -> do
    let pos = i * srcSize
    copyPrimArray darr pos sarr 0 srcSize
  pure darr

concatPrimArray :: (Prim a) => [PrimArray a] -> PrimArray a
concatPrimArray = \case
  [] -> emptyPrimArray
  [s0] -> s0
  ss -> runPrimArray $ do
    let totLen = getSum (foldMap (Sum . sizeofPrimArray) ss)
    darr <- newPrimArray totLen
    offRef <- newSTRef 0
    for_ ss $ \sarr -> do
      let len = sizeofPrimArray sarr
      off <- readSTRef offRef
      copyPrimArray darr off sarr 0 len
      writeSTRef offRef (off + len)
    pure darr

mergeIntoPrimArray
  :: (PrimMonad m, Prim a) => (a -> a -> a) -> MutablePrimArray (PrimState m) a -> Int -> PrimArray a -> Int -> Int -> m ()
mergeIntoPrimArray f darr doff sarr soff slen =
  for_ [0 .. slen - 1] $ \pos -> do
    let dpos = doff + pos
        spos = soff + pos
    val0 <- readPrimArray darr dpos
    let val1 = indexPrimArray sarr spos
    writePrimArray darr dpos (f val0 val1)

mergeMutableIntoPrimArray
  :: (PrimMonad m, Prim a)
  => (a -> a -> a)
  -> MutablePrimArray (PrimState m) a
  -> Int
  -> MutablePrimArray (PrimState m) a
  -> Int
  -> Int
  -> m ()
mergeMutableIntoPrimArray f darr doff sarr soff slen =
  for_ [0 .. slen - 1] $ \pos -> do
    let dpos = doff + pos
        spos = soff + pos
    val0 <- readPrimArray darr dpos
    val1 <- readPrimArray sarr spos
    writePrimArray darr dpos (f val0 val1)

mergePrimArray :: (Prim a, Num a) => (a -> a -> a) -> [PrimArray a] -> PrimArray a
mergePrimArray f = \case
  [] -> emptyPrimArray
  [s0] -> s0
  ss -> runPrimArray $ do
    let totLen = getMax (foldMap (Max . sizeofPrimArray) ss)
    darr <- newPrimArray totLen
    setPrimArray darr 0 totLen 0
    for_ ss $ \sarr -> do
      let len = sizeofPrimArray sarr
      mergeIntoPrimArray f darr 0 sarr 0 len
    pure darr

-- MONO only
i32ByteToElemCount :: ByteCount -> ElemCount
i32ByteToElemCount = ElemCount . (4 *) . unByteCount

-- MONO only
i32ElemToByteCount :: ElemCount -> ByteCount
i32ElemToByteCount = ByteCount . (`div` 4) . unElemCount

newtype InternalSamples = InternalSamples {unInternalSamples :: PrimArray Int32}
  deriving stock (Eq, Show)

instance NFData InternalSamples where
  rnf = rnf . unInternalSamples

isampsEmpty :: InternalSamples
isampsEmpty = InternalSamples emptyPrimArray

isampsIsNull :: InternalSamples -> Bool
isampsIsNull = (0 ==) . isampsBytes

isampsLength :: InternalSamples -> ElemCount
isampsLength = ElemCount . sizeofPrimArray . unInternalSamples

isampsBytes :: InternalSamples -> ByteCount
isampsBytes = i32ElemToByteCount . isampsLength

isampsIndex :: InternalSamples -> ElemCount -> Int32
isampsIndex s = indexPrimArray (unInternalSamples s) . unElemCount

isampsConstant :: ElemCount -> Int32 -> InternalSamples
isampsConstant len = InternalSamples . replicatePrimArray (unElemCount len)

isampsReplicate :: Int -> InternalSamples -> InternalSamples
isampsReplicate n = InternalSamples . replicateWholePrimArray n . unInternalSamples

isampsFromList :: [Int32] -> InternalSamples
isampsFromList = InternalSamples . primArrayFromList

isampsConcat :: [InternalSamples] -> InternalSamples
isampsConcat = InternalSamples . concatPrimArray . fmap unInternalSamples

isampsMix :: [InternalSamples] -> InternalSamples
isampsMix = InternalSamples . mergePrimArray (+) . fmap unInternalSamples

isampsTrim :: ElemCount -> ElemCount -> InternalSamples -> InternalSamples
isampsTrim off len s = InternalSamples (clonePrimArray (unInternalSamples s) (unElemCount off) (unElemCount len))

isampsMap :: (Int32 -> Int32) -> InternalSamples -> InternalSamples
isampsMap f = InternalSamples . mapPrimArray f . unInternalSamples

isampsFill :: ElemCount -> ElemCount -> Int32 -> InternalSamples -> InternalSamples
isampsFill off len val (InternalSamples sarr) = InternalSamples $ runPrimArray $ do
  let tot = ElemCount (sizeofPrimArray sarr)
      lim = off + len
      left = tot - lim
  darr <- newPrimArray (unElemCount tot)
  when (off > 0) (copyPrimArray darr 0 sarr 0 (unElemCount off))
  setPrimArray darr (unElemCount off) (unElemCount len) val
  when (left > 0) (copyPrimArray darr (unElemCount lim) sarr (unElemCount lim) (unElemCount left))
  pure darr

isampsToWave :: InternalSamples -> WAVESamples
isampsToWave = WAVESamples . QuietLiftedArray . LiftedPrimArray . (\(PrimArray x) -> ByteArray x) . unInternalSamples

isampsFromWave :: WAVESamples -> InternalSamples
isampsFromWave = InternalSamples . (\(ByteArray x) -> PrimArray x) . unLiftedPrimArray . unQuietLiftedArray . unWAVESamples

data SampleStream = SampleStream
  { ssFixed :: !InternalSamples
  , ssRepeated :: !InternalSamples
  }
  deriving stock (Eq, Show)

streamRun :: SampleStream -> ElemCount -> Int32
streamRun (SampleStream fixed repeated) =
  let flen = isampsLength fixed
      rlen = isampsLength repeated
  in  \pos ->
        if
          | pos < 0 -> 0
          | pos < flen ->
              indexPrimArray (unInternalSamples fixed) (unElemCount pos)
          | rlen == 0 -> 0
          | otherwise ->
              indexPrimArray (unInternalSamples repeated) (unElemCount (mod (pos - flen) rlen))

streamToIsamps :: ElemCount -> ElemCount -> SampleStream -> InternalSamples
streamToIsamps off len t =
  InternalSamples (generatePrimArray (unElemCount len) (streamRun t . (off +) . ElemCount))

assertMono :: WAVEHeader -> IO ()
assertMono hdr = unless (waveNumChannels hdr == 1) (fail "sample wav must be mono")

readSamples :: WAVE -> IO InternalSamples
readSamples (WAVE hdr samps) = do
  assertMono hdr
  pure (isampsFromWave samps)

writeSamples :: WAVEHeader -> InternalSamples -> IO WAVE
writeSamples hdr isamps = do
  assertMono hdr
  pure (WAVE hdr (isampsToWave isamps))

loadSamples :: FilePath -> IO InternalSamples
loadSamples = getWAVEFile >=> readSamples

loadDataSamples :: String -> InternalSamples
loadDataSamples name = unsafePerformIO (getDataFileName ("data/" ++ name ++ ".wav") >>= loadSamples)

snareSamples :: InternalSamples
snareSamples = loadDataSamples "snare"

clapSamples :: InternalSamples
clapSamples = loadDataSamples "clap"

dumpSamples :: InternalSamples -> IO WAVE
dumpSamples isamps = writeSamples hdr isamps
 where
  hdr =
    WAVEHeader
      { waveNumChannels = 1
      , waveFrameRate = 48000
      , waveBitsPerSample = 16
      , waveFrames = Just (unElemCount (isampsLength isamps))
      }

dumpAllSamples :: IO ()
dumpAllSamples = do
  let items = [] :: [(String, InternalSamples)]
  for_ items $ \(name, samps) -> dumpSamples samps >>= putWAVEFile ("data/" ++ name ++ ".wav")

data OpF n r
  = OpEmpty
  | OpSamp !InternalSamples
  | -- | Invariant: length in (0, inf)
    OpBound !ElemCount r
  | -- | Invariant: length in (0, inf)
    OpSkip !ElemCount r
  | OpRepeat r
  | -- | Invariant: repetitions in (0, inf)
    OpReplicate !Int r
  | OpConcat !(Seq r)
  | OpMerge !(Seq r)
  | OpRef !n
  deriving stock (Eq, Show, Functor, Foldable, Traversable)

type Op n = Fix (OpF n)

opRefs :: (Ord n) => Op n -> Set n
opRefs = cata $ \case
  OpRef n -> Set.singleton n
  op -> fold op

opInferLenF :: (n -> Maybe ElemCount) -> OpF n (Maybe ElemCount) -> Maybe ElemCount
opInferLenF onRef = \case
  OpEmpty -> Just 0
  OpSamp s -> Just (isampsLength s)
  OpBound l _ -> Just l
  OpSkip l r -> fmap (max 0 . subtract l) r
  OpRepeat _ -> Nothing
  OpReplicate n r -> fmap (fromIntegral n *) r
  OpConcat rs -> fmap sum (sequence rs)
  OpMerge rs ->
    let qs = rs >>= maybe Empty (:<| Empty)
    in  case qs of
          Empty -> Nothing
          _ -> Just (maximum qs)
  OpRef n -> onRef n

opInferLen :: (n -> Maybe ElemCount) -> Op n -> Maybe ElemCount
opInferLen = cata . opInferLenF

opInferLenTopo :: (Ord n) => Map n (Op n) -> Either (TopoErr n) (Map n (Maybe ElemCount))
opInferLenTopo m = topoEval opRefs m opInferLen

data Op2F n r
  = Op2Empty
  | Op2Samp !InternalSamples
  | Op2Skip !ElemCount r
  | Op2Replicate !Int r
  | Op2Concat !(Seq r)
  | Op2Merge !(Seq r)
  | Op2Ref !n
  deriving stock (Eq, Show, Functor)

type Op2Anno o n = Memo (Op2F n) o

type LenM n = ReaderT (Map n (Maybe ElemCount)) (State (Map n (Max ElemCount)))

runLenM :: Map n (Maybe ElemCount) -> Map n (Max ElemCount) -> LenM n a -> (a, Map n (Max ElemCount))
runLenM r s m = runState (runReaderT m r) s

-- Given available length and original definition,
-- Returns (calculated length, annotated translation).
-- Properties:
-- 1. If it's possible to infer a length, it will be GTE the calculated length.
-- 2. The calculated length will be LTE available length.
opAnnoLen :: (Ord n) => ElemCount -> Op n -> LenM n (Op2Anno ElemCount n)
opAnnoLen i (Fix opf) = case opf of
  OpEmpty -> do
    pure (MemoP 0 Op2Empty)
  OpSamp s -> do
    let l = min i (isampsLength s)
    pure (MemoP l (Op2Samp s))
  OpBound b r -> do
    let l = min i b
    r' <- opAnnoLen l r
    let MemoP ml f = r'
    pure (MemoP l (if ml == l then f else Op2Concat (r' :<| Empty)))
  OpSkip b r -> do
    r' <- opAnnoLen i r
    let MemoP ml _ = r'
    pure $
      if b >= ml
        then MemoP 0 Op2Empty
        else MemoP (ml - b) (Op2Skip b r')
  OpRepeat r -> do
    r' <- opAnnoLen i r
    let MemoP ml _ = r'
    if ml == 0
      then pure (MemoP 0 Op2Empty)
      else do
        let (n, m) = divMod i ml
            f' = Op2Replicate (unElemCount n) r'
        if m == 0
          then pure (MemoP i f')
          else do
            let q = MemoP (ml * n) f'
            q' <- opAnnoLen m r
            let MemoP ml' _ = q'
            pure (MemoP (ml * n + ml') (Op2Concat (q :<| q' :<| Empty)))
  OpReplicate n r -> do
    let l = div i (ElemCount n)
    r' <- opAnnoLen l r
    let MemoP ml _ = r'
    pure (MemoP (ml * ElemCount n) (Op2Replicate n r'))
  OpConcat rs -> do
    let g !tot !qs = \case
          Empty -> pure (MemoP tot (Op2Concat qs))
          p :<| ps -> do
            let left = i - tot
            if left > 0
              then do
                q <- opAnnoLen left p
                let MemoP ml _ = q
                g (tot + ml) (qs :|> q) ps
              else pure (MemoP tot (Op2Concat qs))
    g 0 Empty rs
  OpMerge rs -> do
    ps <- traverse (opAnnoLen i) rs
    let ml = case fmap memoKey (toList ps) of
          [] -> i
          xs -> maximum xs
    pure (MemoP ml (Op2Merge ps))
  OpRef n -> do
    mx <- asks (join . Map.lookup n)
    l <- case mx of
      Just x -> pure (min i x)
      Nothing -> i <$ modify' (Map.alter (Just . maybe (Max i) (<> Max i)) n)
    pure (MemoP l (Op2Ref n))

data AnnoErr n
  = AnnoErrTopo !(TopoErr n)
  | AnnoErrOrphan !n
  deriving stock (Eq, Ord, Show)

instance (Show n, Typeable n) => Exception (AnnoErr n)

data AnnoSt n = AnnoSt
  { asSpaceNeed :: !(Map n (Max ElemCount))
  , asAnnoOps :: !(Map n (Op2Anno ElemCount n))
  }

type AnnoM n = ReaderT (Map n (Maybe ElemCount)) (StateT (AnnoSt n) (Except (AnnoErr n)))

execAnnoM
  :: Map n (Maybe ElemCount)
  -> Map n (Max ElemCount)
  -> Map n (Op2Anno ElemCount n)
  -> AnnoM n ()
  -> Either (AnnoErr n) (Map n (Op2Anno ElemCount n))
execAnnoM r s1 s2 m = fmap asAnnoOps (runExcept (execStateT (runReaderT m r) (AnnoSt s1 s2)))

lenToAnnoM :: LenM n a -> AnnoM n a
lenToAnnoM m = do
  r <- ask
  s <- gets asSpaceNeed
  let (a, s') = runLenM r s m
  modify' (\as -> as {asSpaceNeed = s'})
  pure a

-- | Annotate all ops with lengths, inferring bottom-up then top-down.
opAnnoLenTopo
  :: forall n. (Ord n) => Map n (Op n) -> Map n (Maybe ElemCount) -> Either (AnnoErr n) (Map n (Op2Anno ElemCount n))
opAnnoLenTopo m n = execAnnoM n Map.empty Map.empty $ do
  let calc k val = do
        let op = m Map.! k
        anno <- lenToAnnoM (opAnnoLen val op)
        modify' (\as -> as {asAnnoOps = Map.insert k anno (asAnnoOps as)})
  -- Calculate reverse topo order (top down)
  ks <-
    either
      (throwError . AnnoErrTopo)
      (pure . Seq.reverse)
      (topoSort (fmap opRefs . flip Map.lookup m) (Map.keys m))
  -- First round - if it has an inferred len, annotate.
  for_ ks $ \k -> do
    minf <- asks (join . Map.lookup k)
    for_ minf (calc k)
  -- Second round - if found a new len, annotate. Otherwise fail.
  for_ ks $ \k -> do
    minf <- asks (join . Map.lookup k)
    case minf of
      Just _ -> pure ()
      Nothing -> do
        mfound <- gets (Map.lookup k . asSpaceNeed)
        case mfound of
          Nothing -> throwError (AnnoErrOrphan k)
          Just found -> calc k (getMax found)

data OpEnv n = OpEnv
  { oeDefs :: !(Map n (Op2Anno ElemCount n))
  , oeFutVar :: !(MVar (Map n (IVar (PrimArray Int32))))
  , oeOff :: !ElemCount
  , oeBufVar :: !(MVar (MutablePrimArray RealWorld Int32))
  }
  deriving stock (Eq)

type OpM n = ReadPar (OpEnv n)

execOpM :: Map n (Op2Anno ElemCount n) -> ElemCount -> OpM n () -> IO (PrimArray Int32)
execOpM defs len act = do
  buf <- newPrimArray (unElemCount len)
  bufVar <- newMVar buf
  futVar <- newMVar Map.empty
  let env = OpEnv {oeDefs = defs, oeFutVar = futVar, oeOff = 0, oeBufVar = bufVar}
  runReadPar act env
  unsafeFreezePrimArray buf

handleOpSamp :: ElemCount -> InternalSamples -> OpM n ()
handleOpSamp clen (InternalSamples src) = do
  OpEnv _ _ off bufVar <- ask
  liftIO $ withMVar bufVar $ \b -> copyPrimArray b (unElemCount off) src 0 (unElemCount clen)

-- TODO pass start offset recursively to avoid generating useless prefix
handleOpSkip :: (Ord n) => ElemCount -> ElemCount -> Op2Anno ElemCount n -> OpM n ()
handleOpSkip clen k r = do
  let rlen = memoKey r
  tmp <- liftIO (zeroPrimArray (unElemCount rlen))
  tmpVar <- liftIO (newMVar tmp)
  local (\env -> env {oeOff = 0, oeBufVar = tmpVar}) (goOpToWave r)
  OpEnv _ _ off bufVar <- ask
  liftIO $ withMVar bufVar $ \b ->
    copyMutablePrimArray b (unElemCount off) b (unElemCount k) (unElemCount clen)

handleOpReplicate :: (Ord n) => ElemCount -> Int -> Op2Anno ElemCount n -> OpM n ()
handleOpReplicate clen n r = do
  OpEnv _ _ off bufVar <- ask
  goOpToWave r
  liftIO $ withMVar bufVar $ \b -> do
    for_ [1 .. n - 1] $ \i -> do
      let off' = off + ElemCount i * clen
      copyMutablePrimArray b (unElemCount off') b (unElemCount off) (unElemCount clen)

handleOpConcat :: (Ord n) => ElemCount -> Seq (Op2Anno ElemCount n) -> OpM n ()
handleOpConcat _ rs = do
  off <- asks oeOff
  parFor (InclusiveRange 0 (length rs - 1)) $ \i -> do
    let r = Seq.index rs i
        elemOff = sum (take i (map memoKey (toList rs)))
        off' = off + elemOff
    local (\env -> env {oeOff = off'}) (goOpToWave r)

handleOpMerge :: (Ord n) => ElemCount -> Seq (Op2Anno ElemCount n) -> OpM n ()
handleOpMerge _ rs = do
  parFor (InclusiveRange 0 (length rs - 1)) $ \i -> do
    let r = Seq.index rs i
        rlen = memoKey r
    tmp <- liftIO (zeroPrimArray (unElemCount rlen))
    tmpVar <- liftIO (newMVar tmp)
    local (\env -> env {oeOff = 0, oeBufVar = tmpVar}) (goOpToWave r)
    OpEnv _ _ off bufVar <- ask
    liftIO $ withMVar bufVar $ \b ->
      mergeMutableIntoPrimArray (+) b (unElemCount off) tmp 0 (unElemCount rlen)

handleOpRef :: (Ord n) => ElemCount -> n -> OpM n ()
handleOpRef clen n = do
  OpEnv defs futVar off bufVar <- ask
  let def = defs Map.! n
  ivar <- Par.new
  xvar <- liftIO $ modifyMVar futVar $ \fut -> do
    pure $ case Map.lookup n fut of
      Nothing -> (Map.insert n ivar fut, Nothing)
      Just var -> (fut, Just var)
  val <- case xvar of
    Nothing -> do
      void $ Par.spawn_ $ Par.put_ ivar =<< liftIO (execOpM defs clen (goOpToWave def))
      Par.get ivar
    Just yvar -> Par.get yvar
  liftIO $ withMVar bufVar $ \b ->
    copyPrimArray b (unElemCount off) val 0 (unElemCount clen)

goOpToWave :: (Ord n) => Op2Anno ElemCount n -> OpM n ()
goOpToWave (MemoP clen f) = case f of
  Op2Empty -> pure ()
  Op2Samp s -> handleOpSamp clen s
  Op2Skip b r -> handleOpSkip clen b r
  Op2Replicate n r -> handleOpReplicate clen n r
  Op2Concat rs -> handleOpConcat clen rs
  Op2Merge rs -> handleOpMerge clen rs
  Op2Ref n -> handleOpRef clen n

data OpErr n
  = OpErrInfer !(TopoErr n)
  | OpErrAnno !(AnnoErr n)
  | OpErrRoot !n
  | OpErrOverflow
  deriving stock (Eq, Ord, Show)

instance (Show n, Typeable n) => Exception (OpErr n)

opsToWave :: (Ord n) => Map n (Op n) -> n -> IO (Either (OpErr n) InternalSamples)
opsToWave defs n = runExceptT $ do
  infs <- either (throwError . OpErrInfer) pure (opInferLenTopo defs)
  anns <- either (throwError . OpErrAnno) pure (opAnnoLenTopo defs infs)
  root <- maybe (throwError (OpErrRoot n)) pure (Map.lookup n anns)
  arr <- liftIO $ execOpM anns (memoKey root) (goOpToWave root)
  pure (InternalSamples arr)

opToWave :: (Ord n) => n -> Op n -> IO (Either (OpErr n) InternalSamples)
opToWave n op = opsToWave (Map.singleton n op) n
