module Data.Sounds where

import Bowtie (Fix (..), Memo, memoKey, pattern MemoP)
import Control.Concurrent.MVar (MVar, modifyMVar, newMVar, withMVar)
import Control.DeepSeq (NFData)
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
import Data.Foldable (fold, for_, toList, traverse_)
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
  deriving newtype (NFData)

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

data Extent = Extent
  { extOff :: !ElemCount
  , extLen :: !ElemCount
  }
  deriving stock (Eq, Show)

instance Semigroup Extent where
  Extent o1 l1 <> Extent o2 l2 =
    let x1 = o1 + l1
        x2 = o2 + l2
        o3 = min o1 o2
        l3 = max x1 x2 - o3
    in  Extent o3 l3

extEmpty :: Extent
extEmpty = Extent 0 0

extNull :: Extent -> Bool
extNull (Extent _ l) = l <= 0

data Op2F n r
  = Op2Empty
  | Op2Samp !Extent !InternalSamples
  | Op2Replicate !Int r
  | Op2Concat !(Seq r)
  | Op2Merge !(Seq r)
  | Op2Ref !Extent !n
  deriving stock (Eq, Show, Functor, Foldable, Traversable)

data ValErr n
  = -- | Expected length, actual sum of child lengths
    ValErrChildLen !ElemCount !ElemCount
  | -- | Expected length, actual extent length
    ValErrExtentLen !ElemCount !ElemCount
  | -- | Non-zero length for Op2Empty
    ValErrEmptyLen !ElemCount
  | -- | Negative offset in extent
    ValErrNegativeExtentOffset !ElemCount
  | -- | Negative length in extent
    ValErrNegativeExtentLength !ElemCount
  | -- | Non-positive number of repetitions
    ValErrNonPositiveReps !Int
  | -- | No children in Op2Concat or Op2Merge
    ValErrNoChildren
  | -- | Reference not found
    ValErrRef !n
  deriving stock (Eq, Show)

op2Validate :: (n -> Bool) -> Op2Anno n -> Either (ValErr n) ()
op2Validate onRef = runExcept . go
 where
  go (MemoP totLen op) = case op of
    Op2Empty ->
      unless (totLen == 0) $ throwError (ValErrEmptyLen totLen)
    Op2Samp (Extent off len) _ -> do
      unless (off >= 0) $ throwError (ValErrNegativeExtentOffset off)
      unless (len >= 0) $ throwError (ValErrNegativeExtentLength len)
      unless (totLen == len) $ throwError (ValErrExtentLen totLen len)
    Op2Replicate n r -> do
      go r
      unless (n > 0) $ throwError (ValErrNonPositiveReps n)
      let MemoP rLen _ = r
          childSum = rLen * ElemCount n
      unless (childSum <= totLen) $ throwError (ValErrChildLen totLen childSum)
    Op2Concat rs -> do
      traverse_ go rs
      when (Seq.null rs) $ throwError ValErrNoChildren
      let childSum = sum (fmap memoKey rs)
      unless (childSum <= totLen) $ throwError (ValErrChildLen totLen childSum)
    Op2Merge rs -> do
      traverse_ go rs
      when (Seq.null rs) $ throwError ValErrNoChildren
      let childSum = maximum (fmap memoKey rs)
      unless (childSum <= totLen) $ throwError (ValErrChildLen totLen childSum)
    Op2Ref (Extent off len) n -> do
      unless (off >= 0) $ throwError (ValErrNegativeExtentOffset off)
      unless (len >= 0) $ throwError (ValErrNegativeExtentLength len)
      unless (totLen == len) $ throwError (ValErrExtentLen totLen len)
      unless (onRef n) $ throwError (ValErrRef n)

type Op2Anno n = Memo (Op2F n) ElemCount

op2Empty :: Op2Anno n
op2Empty = MemoP 0 Op2Empty

op2Null :: Op2Anno n -> Bool
op2Null = \case
  MemoP _ Op2Empty -> True
  MemoP len _ -> len <= 0

op2NullNorm :: Op2Anno n -> Op2Anno n
op2NullNorm = \case
  an@(MemoP _ Op2Empty) -> an
  MemoP len _ | len <= 0 -> op2Empty
  an -> an

type LenM n = ReaderT (Map n (Maybe ElemCount)) (State (Map n Extent))

runLenM :: Map n (Maybe ElemCount) -> Map n Extent -> LenM n a -> (a, Map n Extent)
runLenM r s m = runState (runReaderT m r) s

-- Given available length and original definition,
-- Returns (calculated length, annotated translation).
-- Properties:
-- 1. If it's possible to infer a length, it will be GTE the calculated length.
-- 2. The calculated length will be LTE available length.
opAnnoLen :: (Ord n) => Extent -> Op n -> LenM n (Op2Anno n)
opAnnoLen = go
 where
  go ext op =
    if extNull ext
      then pure op2Empty
      else fmap op2NullNorm (goOp ext op)
  goOp haveExt@(Extent haveOff haveLen) (Fix opf) = case opf of
    OpEmpty -> pure op2Empty
    OpSamp s ->
      let len' = min haveLen (isampsLength s - haveOff)
      in  pure (MemoP len' (Op2Samp (Extent haveOff len') s))
    OpBound include r -> do
      let len' = min haveLen include
      r' <- go (Extent haveOff len') r
      let MemoP len'' f = r'
      pure (MemoP len' (if len'' == len' then f else Op2Concat (r' :<| Empty)))
    OpSkip exclude r ->
      let len' = max 0 (haveLen - exclude)
      in  go (Extent (haveOff + exclude) len') r
    OpRepeat r -> do
      r' <- go (Extent 0 (haveLen + haveOff)) r
      let MemoP len' _ = r'
      if len' <= 0
        then pure op2Empty
        else do
          let preOff = mod haveOff len'
              preLen = if preOff > 0 then len' - preOff else 0
              (numBody, postLen) = divMod (haveLen - preLen) len'
              bodyLen = len' * numBody
              body = if bodyLen > 0 then Just (MemoP bodyLen (Op2Replicate (unElemCount numBody) r')) else Nothing
          pre <-
            if preLen > 0
              then fmap (\x -> if op2Null x then Nothing else Just x) (go (Extent preOff preLen) r)
              else pure Nothing
          post <-
            if postLen > 0
              then fmap (\x -> if op2Null x then Nothing else Just x) (go (Extent 0 postLen) r)
              else pure Nothing
          let qs = maybe Empty Seq.singleton pre <> maybe Empty Seq.singleton body <> maybe Empty Seq.singleton post
              qsLen = maybe 0 memoKey pre + bodyLen + maybe 0 memoKey post
          case qs of
            Empty -> pure op2Empty
            q :<| Empty -> pure q
            _ -> pure (MemoP qsLen (Op2Concat qs))
    OpReplicate n r -> do
      r' <- go (Extent 0 (haveLen + haveOff)) r
      let MemoP len' _ = r'
      if len' <= 0
        then pure op2Empty
        else do
          let (ElemCount numPre, preOff) = divMod haveOff len'
              preLen = if preOff > 0 then len' - preOff else 0
              numFull = unElemCount (div (haveLen + haveOff) len')
              numBody = min n numFull - numPre
              bodyLen = len' * ElemCount numBody
              postLen = if numBody < n then haveLen - (preLen + bodyLen) else 0
              body = if bodyLen > 0 then Just (MemoP bodyLen (Op2Replicate numBody r')) else Nothing
          pre <-
            if preLen > 0
              then fmap (\x -> if op2Null x then Nothing else Just x) (go (Extent preOff preLen) r)
              else pure Nothing
          post <-
            if postLen > 0
              then fmap (\x -> if op2Null x then Nothing else Just x) (go (Extent 0 postLen) r)
              else pure Nothing
          let qs = maybe Empty Seq.singleton pre <> maybe Empty Seq.singleton body <> maybe Empty Seq.singleton post
              qsLen = maybe 0 memoKey pre + bodyLen + maybe 0 memoKey post
          case qs of
            Empty -> pure op2Empty
            q :<| Empty -> pure q
            _ -> pure (MemoP qsLen (Op2Concat qs))
    OpConcat rs -> do
      let process !tot !qs = \case
            Empty -> pure (MemoP tot (Op2Concat qs))
            p :<| ps -> do
              let left = haveLen - tot
              if left > 0
                then do
                  q <- go (Extent 0 left) p
                  let MemoP len _ = q
                  process (tot + len) (qs :|> q) ps
                else pure (MemoP tot (Op2Concat qs))
      if haveOff == 0
        then process 0 Empty rs
        else do
          -- Find the subtree containing our offset
          let findOffset !tot = \case
                Empty -> pure (0, Nothing, Empty)
                p :<| ps -> do
                  -- Calculate length of subtree without offset
                  q <- go (Extent 0 (haveLen + haveOff - tot)) p
                  let MemoP len _ = q
                  if tot + len <= haveOff
                    then findOffset (tot + len) ps
                    else do
                      -- Found the subtree containing our offset
                      let subOffset = haveOff - tot
                      q' <- go (Extent subOffset haveLen) p
                      let MemoP len' _ = q'
                      pure (len', Just q', ps)
          (tot, partial, rest) <- findOffset 0 rs
          process tot (maybe Empty Seq.singleton partial) rest
    OpMerge rs -> do
      ps <- fmap (Seq.filter (not . op2Null)) (traverse (go haveExt) rs)
      case ps of
        Empty -> pure op2Empty
        p :<| Empty -> pure p
        _ -> do
          let maxLen = maximum (fmap memoKey (toList ps))
          pure (MemoP maxLen (Op2Merge ps))
    OpRef n -> do
      mx <- asks (join . Map.lookup n)
      len' <- case mx of
        Just x -> pure (max 0 (min x (haveLen + haveOff) - haveOff))
        Nothing -> haveLen <$ modify' (Map.alter (Just . maybe haveExt (<> haveExt)) n)
      pure (MemoP len' (Op2Ref (Extent haveOff len') n))

data AnnoErr n
  = AnnoErrTopo !(TopoErr n)
  | AnnoErrOrphan !n
  deriving stock (Eq, Ord, Show)

instance (Show n, Typeable n) => Exception (AnnoErr n)

data AnnoSt n = AnnoSt
  { asSpaceNeed :: !(Map n Extent)
  , asAnnoOps :: !(Map n (Op2Anno n))
  }

type AnnoM n = ReaderT (Map n (Maybe ElemCount)) (StateT (AnnoSt n) (Except (AnnoErr n)))

execAnnoM
  :: Map n (Maybe ElemCount)
  -> Map n Extent
  -> Map n (Op2Anno n)
  -> AnnoM n ()
  -> Either (AnnoErr n) (Map n (Op2Anno n))
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
  :: forall n. (Ord n) => Map n (Op n) -> Map n (Maybe ElemCount) -> Either (AnnoErr n) (Map n (Op2Anno n))
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
    for_ minf (calc k . Extent 0)
  -- Second round - if found a new len, annotate. Otherwise fail.
  for_ ks $ \k -> do
    minf <- asks (join . Map.lookup k)
    case minf of
      Just _ -> pure ()
      Nothing -> do
        mfound <- gets (Map.lookup k . asSpaceNeed)
        case mfound of
          Nothing -> throwError (AnnoErrOrphan k)
          Just found -> calc k found

data OpEnv n = OpEnv
  { oeDefs :: !(Map n (Op2Anno n))
  , oeFutVar :: !(MVar (Map n (IVar (PrimArray Int32))))
  , oeOff :: !ElemCount
  , oeBufVar :: !(MVar (MutablePrimArray RealWorld Int32))
  }
  deriving stock (Eq)

type OpM n = ReadPar (OpEnv n)

execOpM :: Map n (Op2Anno n) -> ElemCount -> OpM n () -> IO (PrimArray Int32)
execOpM defs len act = do
  buf <- newPrimArray (unElemCount len)
  bufVar <- newMVar buf
  futVar <- newMVar Map.empty
  let env = OpEnv {oeDefs = defs, oeFutVar = futVar, oeOff = 0, oeBufVar = bufVar}
  runReadPar act env
  unsafeFreezePrimArray buf

handleOpSamp :: ElemCount -> ElemCount -> InternalSamples -> OpM n ()
handleOpSamp coff clen (InternalSamples src) = do
  OpEnv _ _ off bufVar <- ask
  liftIO $ withMVar bufVar $ \b -> copyPrimArray b (unElemCount off) src (unElemCount coff) (unElemCount clen)

handleOpReplicate :: (Ord n) => Int -> Op2Anno n -> OpM n ()
handleOpReplicate n r = do
  OpEnv _ _ off bufVar <- ask
  let MemoP rlen _ = r
  goOpToWave r
  liftIO $ withMVar bufVar $ \b -> do
    for_ [1 .. n - 1] $ \i -> do
      let off' = off + ElemCount i * rlen
      copyMutablePrimArray b (unElemCount off') b (unElemCount off) (unElemCount rlen)

handleOpConcat :: (Ord n) => Seq (Op2Anno n) -> OpM n ()
handleOpConcat rs = do
  off <- asks oeOff
  parFor (InclusiveRange 0 (length rs - 1)) $ \i -> do
    let r = Seq.index rs i
        elemOff = sum (take i (map memoKey (toList rs)))
        off' = off + elemOff
    local (\env -> env {oeOff = off'}) (goOpToWave r)

handleOpMerge :: (Ord n) => Seq (Op2Anno n) -> OpM n ()
handleOpMerge rs = do
  parFor (InclusiveRange 0 (length rs - 1)) $ \i -> do
    let r = Seq.index rs i
        rlen = memoKey r
    tmp <- liftIO (zeroPrimArray (unElemCount rlen))
    tmpVar <- liftIO (newMVar tmp)
    local (\env -> env {oeOff = 0, oeBufVar = tmpVar}) (goOpToWave r)
    OpEnv _ _ off bufVar <- ask
    liftIO $ withMVar bufVar $ \b ->
      mergeMutableIntoPrimArray (+) b (unElemCount off) tmp 0 (unElemCount rlen)

handleOpRef :: (Ord n) => ElemCount -> ElemCount -> n -> OpM n ()
handleOpRef _coff clen n = do
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

goOpToWave :: (Ord n) => Op2Anno n -> OpM n ()
goOpToWave (MemoP clen f) = case f of
  Op2Empty -> pure ()
  Op2Samp (Extent coff _) s -> handleOpSamp coff clen s
  Op2Replicate n r -> handleOpReplicate n r
  Op2Concat rs -> handleOpConcat rs
  Op2Merge rs -> handleOpMerge rs
  Op2Ref (Extent coff _) n -> handleOpRef coff clen n

data OpErr n
  = OpErrInfer !(TopoErr n)
  | OpErrAnno !(AnnoErr n)
  | OpErrRoot !n
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
