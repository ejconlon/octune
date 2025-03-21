module Data.Sounds where

import Control.DeepSeq (NFData (..))
import Control.Exception (Exception)
import Control.Monad (unless, when, (>=>))
import Control.Monad.Primitive (PrimMonad (..), RealWorld)
import Control.Monad.Reader (Reader, ask, asks, local, runReader)
import Dahdit.Audio.Binary (QuietLiftedArray (..))
import Dahdit.Audio.Wav.Simple (WAVE (..), WAVEHeader (..), WAVESamples (..), getWAVEFile, putWAVEFile)
import Dahdit.LiftedPrimArray (LiftedPrimArray (..))
import Dahdit.Sizes (ByteCount (..), ElemCount (..))
import Data.Foldable (for_, toList)
import Data.Int (Int32)
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
import Paths_octune (getDataFileName)
import System.IO.Unsafe (unsafePerformIO)

-- Prim adapters

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

data OpF r
  = OpEmpty
  | OpSamp !InternalSamples
  | -- | Invariant: length is in (0, inf)
    OpBound !ElemCount r
  | -- | Invariant: fraction in (0, 1)
    OpCut !Rational r
  | OpRepeat r
  | -- | Invariant: repetitions in (0, inf)
    OpReplicate !Int r
  | OpConcat !(Seq r)
  | OpMerge !(Seq r)
  deriving stock (Eq, Show, Functor)

newtype Op = Op {unOp :: OpF Op}
  deriving stock (Eq, Show)

cataOp :: (OpF r -> r) -> Op -> r
cataOp f = go
 where
  go = f . fmap go . unOp

opInferLenF :: OpF (Maybe ElemCount) -> Maybe ElemCount
opInferLenF = \case
  OpEmpty -> Nothing
  OpSamp s -> Just (isampsLength s)
  OpBound l _ -> Just l
  OpCut _ r -> r
  OpRepeat _ -> Nothing
  OpReplicate n r -> fmap (fromIntegral n *) r
  OpConcat rs -> fmap sum (sequence rs)
  OpMerge rs -> fmap maximum (sequence rs)

opInferLen :: Op -> Maybe ElemCount
opInferLen = cataOp opInferLenF

data Anno k v = Anno {annoKey :: k, annoVal :: v}
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

data Op2F r
  = Op2Empty
  | Op2Samp !InternalSamples
  | Op2Replicate !Int r
  | Op2Concat !(Seq r)
  | Op2Merge !(Seq r)
  deriving stock (Eq, Show, Functor)

newtype Op2Anno k = Op2Anno {unOp2Anno :: Anno k (Op2F (Op2Anno k))}
  deriving stock (Eq, Show)

cataOp2Anno :: (Op2F r -> Reader k r) -> Op2Anno k -> r
cataOp2Anno f = go
 where
  go (Op2Anno (Anno k v)) = runReader (f (fmap go v)) k

-- Given available length and original definition,
-- Returns (calculated length, annotated translation).
-- Properties:
-- 1. If it's possible to infer a length, it will be GTE the calculated length.
-- 2. The calculated length will be LTE available length.
opAnnoLenF :: ElemCount -> OpF Op -> (ElemCount, Op2F (Op2Anno ElemCount))
opAnnoLenF i = \case
  OpEmpty ->
    (0, Op2Empty)
  OpSamp s ->
    let l' = min i (isampsLength s)
    in  (l', Op2Samp s)
  OpBound l r ->
    let l' = min i l
        (ml, f) = opAnnoLenF l' (unOp r)
        q' = Op2Anno (Anno ml f)
    in  (l', if ml == l' then f else Op2Concat (q' :<| Empty))
  OpCut x r ->
    let l' = min i (floor (x * fromIntegral i))
        (ml, f) = opAnnoLenF l' (unOp r)
        q' = Op2Anno (Anno ml f)
    in  (ml, if ml == l' then f else Op2Concat (q' :<| Empty))
  OpRepeat r ->
    let (ml, f) = opAnnoLenF i (unOp r)
        (n, m) = divMod i ml
        r' = Op2Replicate (unElemCount n) (Op2Anno (Anno ml f))
    in  if m == 0
          then
            (i, r')
          else
            let q' = Op2Anno (Anno (ml * n) r')
                (ml', f') = opAnnoLenF m (unOp r)
                q'' = Op2Anno (Anno ml' f')
            in  (ml * n + ml', Op2Concat (q' :<| q'' :<| Empty))
  OpReplicate n r ->
    let l' = div i (ElemCount n)
        (ml, f) = opAnnoLenF l' (unOp r)
    in  (ml * ElemCount n, Op2Replicate n (Op2Anno (Anno ml f)))
  OpConcat rs ->
    let g !tot !qs = \case
          Empty -> (tot, Op2Concat qs)
          p :<| ps ->
            let left = i - tot
            in  if left > 0
                  then
                    let (ml, f) = opAnnoLenF left (unOp p)
                        tot' = tot + ml
                        qs' = qs :|> Op2Anno (Anno ml f)
                    in  g tot' qs' ps
                  else (tot, Op2Concat qs)
    in  g 0 Empty rs
  OpMerge rs ->
    let ps = fmap (opAnnoLenF i . unOp) rs
        ml = maximum (fmap fst (toList ps))
    in  (ml, Op2Merge (fmap (\(ml', f') -> Op2Anno (Anno ml' f')) ps))

opAnnoLen :: ElemCount -> Op -> Op2Anno ElemCount
opAnnoLen i (Op f) = let (ml, f') = opAnnoLenF i f in Op2Anno (Anno ml f')

data OpEnv = OpEnv
  { oeOff :: !ElemCount
  , oeBuf :: !(MutablePrimArray RealWorld Int32)
  }
  deriving stock (Eq)

data OpErr
  = OpErrInfer
  | OpErrOverflow
  deriving stock (Eq, Ord, Show)

instance Exception OpErr

type OpM = ReadPar OpEnv

runOpM :: OpM a -> OpEnv -> IO a
runOpM = runReadPar

execOpM :: ElemCount -> OpM () -> IO (PrimArray Int32)
execOpM len act = do
  buf <- newPrimArray (unElemCount len)
  let env = OpEnv {oeOff = 0, oeBuf = buf}
  runOpM act env
  unsafeFreezePrimArray buf

opToWave :: Op -> IO (Either OpErr WAVESamples)
opToWave op =
  case opInferLen op of
    Nothing -> pure (Left OpErrInfer)
    Just len -> do
      let an = opAnnoLen len op
      arr <- execOpM len (goOpToWave an)
      pure (Right (isampsToWave (InternalSamples arr)))

handleOpSamp :: ElemCount -> InternalSamples -> OpM ()
handleOpSamp clen (InternalSamples src) = do
  OpEnv off buf <- ask
  copyPrimArray buf (unElemCount off) src 0 (unElemCount clen)

handleOpReplicate :: ElemCount -> Int -> Op2Anno ElemCount -> OpM ()
handleOpReplicate clen n r = do
  OpEnv off buf <- ask
  goOpToWave r
  for_ [1 .. n - 1] $ \i -> do
    let off' = off + ElemCount i * clen
    copyMutablePrimArray buf (unElemCount off') buf (unElemCount off) (unElemCount clen)

handleOpConcat :: ElemCount -> Seq (Op2Anno ElemCount) -> OpM ()
handleOpConcat clen rs = do
  off <- asks oeOff
  for_ (zip [0 ..] (toList rs)) $ \(i, r) -> do
    let off' = off + i * clen
    local (\env -> env {oeOff = off'}) $ do
      goOpToWave r

handleOpMerge :: ElemCount -> Seq (Op2Anno ElemCount) -> OpM ()
handleOpMerge clen rs = do
  OpEnv off buf <- ask
  tmp <- newPrimArray (unElemCount clen)
  local (\env -> env {oeOff = 0, oeBuf = tmp}) $ do
    for_ rs $ \r -> do
      setPrimArray tmp 0 (unElemCount clen) 0
      goOpToWave r
      mergeMutableIntoPrimArray (+) buf (unElemCount off) tmp 0 (unElemCount clen)

goOpToWave :: Op2Anno ElemCount -> OpM ()
goOpToWave (Op2Anno (Anno clen f)) = case f of
  Op2Empty -> pure ()
  Op2Samp s -> handleOpSamp clen s
  Op2Replicate n r -> handleOpReplicate clen n r
  Op2Concat rs -> handleOpConcat clen rs
  Op2Merge rs -> handleOpMerge clen rs
