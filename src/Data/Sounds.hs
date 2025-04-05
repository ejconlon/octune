module Data.Sounds where

import Bowtie (Anno (..), Fix (..), Memo, cataM, memoFix, memoKey, mkMemoM, pattern MemoP)
import Control.Applicative (Alternative (..))
import Control.DeepSeq (NFData)
import Control.Monad (unless, void, when, (>=>))
import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Identity (Identity (..))
import Control.Monad.Primitive (PrimMonad (..), RealWorld)
import Control.Monad.Reader (ReaderT (..))
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Dahdit.Audio.Binary (QuietLiftedArray (..))
import Dahdit.Audio.Wav.Simple (WAVE (..), WAVEHeader (..), WAVESamples (..), getWAVEFile, putWAVEFile)
import Dahdit.LiftedPrimArray (LiftedPrimArray (..))
import Dahdit.Sizes (ByteCount (..), ElemCount (..))
import Data.Foldable (fold, foldl', for_, toList)
import Data.Functor.Foldable (Recursive (..), cata)
import Data.Int (Int32)
import Data.Map.Strict (Map)
import Data.PrimPar (Mutex, PrimPar, newMutex, runPrimPar, withMutex)
import Data.Primitive.ByteArray (ByteArray (..))
import Data.Primitive.PrimArray
  ( MutablePrimArray
  , PrimArray (..)
  , clonePrimArray
  , copyPrimArray
  , emptyPrimArray
  , generatePrimArray
  , getSizeofMutablePrimArray
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
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Topo (SortErr (..), topoAnnoM, topoEval)
import Debug.Trace (trace)
import Paths_octune (getDataFileName)
import System.Directory (doesFileExist)
import System.IO.Unsafe (unsafePerformIO)

traceAll :: [[String]] -> a -> a
traceAll xs = if True then id else trace ("\n<==\n" ++ unlines (fmap (("* " ++) . unwords) xs) ++ "==>\n")

-- Bowtie utils

memoRecallM :: (Monad m, Traversable f) => (f (Anno k v) -> ReaderT k m v) -> Memo f k -> m v
memoRecallM f = go
 where
  go (MemoP k fm) = traverse (\m -> fmap (Anno (memoKey m)) (go m)) fm >>= \fa -> runReaderT (f fa) k

-- Prim adapters

zeroPrimArray :: (PrimMonad m, Prim a, Num a) => Int -> m (MutablePrimArray (PrimState m) a)
zeroPrimArray n = do
  marr <- newPrimArray n
  setPrimArray marr 0 n 0
  pure marr

replicateWholePrimArray :: (Prim a) => Int -> PrimArray a -> PrimArray a
replicateWholePrimArray n sarr =
  if
    | n <= 0 -> emptyPrimArray
    | n == 1 -> sarr
    | otherwise -> runPrimArray $ do
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
loadDataSamples name =
  -- Default to resources but fall back to filesystem for ghci
  let part = "data/" ++ name ++ ".wav"
  in  unsafePerformIO $ do
        resPath <- getDataFileName part
        resExists <- doesFileExist resPath
        loadSamples (if resExists then resPath else part)

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

class (Ord t, Ord d, Num d) => Measure t d | t -> d where
  measureDelta :: t -> t -> d
  addDelta :: t -> d -> t

instance Measure ElemCount ElemCount where
  measureDelta c1 c2 = c2 - c1
  addDelta c d = c + d

class (Ord t, Ord r) => Quantize t r | t -> r where
  quantize :: (Integral u) => r -> Arc t -> Arc u
  unquantize :: (Integral u) => r -> Arc u -> Arc t

newtype Time = Time {unTime :: Rational}
  deriving stock (Eq, Show)
  deriving newtype (Num, Ord, Enum, Real, Fractional, RealFrac)

newtype Delta = Delta {unDelta :: Rational}
  deriving stock (Eq, Show)
  deriving newtype (Num, Ord, Enum, Real, Fractional, RealFrac)

-- deriving (Semigroup, Monoid) via (Sum Rational)

instance Measure Time Delta where
  measureDelta (Time t1) (Time t2) = Delta (t2 - t1)
  addDelta (Time t) (Delta d) = Time (t + d)

-- Rate in "things per unit time"
newtype Rate = Rate {unRate :: Rational}
  deriving stock (Eq, Show)
  deriving newtype (Num, Ord, Enum, Real, Fractional, RealFrac)

instance Quantize Time Rate where
  quantize (Rate r) (Arc (Time s) (Time e)) = Arc (truncate (s * r)) (ceiling (e * r))
  unquantize (Rate r) (Arc s e) = Arc (Time (fromIntegral s / r)) (Time (fromIntegral e / r))

newtype Reps = Reps {unReps :: Rational}
  deriving stock (Eq, Show)
  deriving newtype (Num, Ord, Enum, Real, Fractional, RealFrac)

mulReps :: (Fractional d) => Reps -> d -> d
mulReps (Reps n) d = fromRational n * d

-- Arcs are [start, end), where [same, same) is empty
data Arc t = Arc
  { arcStart :: !t
  , arcEnd :: !t
  }
  deriving stock (Eq, Ord, Show)

arcEmpty :: (Num t) => Arc t
arcEmpty = Arc 0 0

arcUnion :: (Ord t) => Arc t -> Arc t -> Arc t
arcUnion (Arc s1 e1) (Arc s2 e2) =
  let s3 = min s1 s2
      e3 = max e1 e2
  in  Arc s3 e3

arcIntersect :: (Measure t d) => Arc t -> Arc t -> Maybe (Arc t)
arcIntersect (Arc s1 e1) (Arc s2 e2) =
  let s3 = max s1 s2
      e3 = min e1 e2
  in  if s2 >= e1 || s1 >= e2 || s3 >= e3 then Nothing else Just (Arc s3 e3)

arcLen :: (Measure t d) => Arc t -> d
arcLen (Arc s e) = measureDelta s e

arcFrom :: (Measure t d) => t -> d -> Arc t
arcFrom t d =
  let u = addDelta t d
  in  if t <= u then Arc t u else Arc u t

arcNull :: (Measure t d) => Arc t -> Bool
arcNull = (0 ==) . arcLen

arcShift :: (Measure t d) => Arc t -> d -> Arc t
arcShift (Arc s e) d = Arc (addDelta s d) (addDelta e d)

arcRepeat :: (Measure t d, Fractional d) => Reps -> Arc t -> Arc t
arcRepeat n (Arc s e) = Arc s (addDelta s (mulReps n (measureDelta s e)))

data Overlap t d = OverlapLt | OverlapOn !d !(Arc t) !d | OverlapGt
  deriving stock (Eq, Ord, Show)

arcOverlap :: (Measure t d) => Arc t -> Arc t -> Overlap t d
arcOverlap (Arc ns ne) (Arc hs he) =
  if
    | ne <= hs -> OverlapLt
    | ns >= he -> OverlapGt
    | otherwise ->
        let xs = max ns hs
            xe = min ne he
            before = measureDelta ns xs
            after = measureDelta xe ne
        in  OverlapOn before (Arc xs xe) after

arcRelative :: (Measure t d, Num t) => Arc t -> Arc t -> Maybe (d, Arc t, d)
arcRelative (Arc ns ne) abso@(Arc hs he) =
  let rel'@(Arc ns' _) = Arc (max 0 ns) (max 0 ne)
      s = addDelta hs (measureDelta 0 ns')
      e = min he (addDelta s (arcLen rel'))
  in  if s >= e
        then Nothing
        else Just (max 0 (measureDelta ns 0), Arc s e, max 0 (arcLen rel' - arcLen abso))

arcSkip :: (Measure t d) => d -> Arc t -> Maybe (Arc t)
arcSkip d (Arc bs be) =
  let s = addDelta bs d
  in  if s >= be
        then Nothing
        else Just (Arc s be)

arcNarrow :: (Measure t d, Num t) => Arc t -> Arc t -> Maybe (Arc t)
arcNarrow rel@(Arc rs _) base@(Arc bs _) =
  let s = bs + rs
      l = min (arcLen rel) (arcLen base)
  in  if l <= 0
        then Nothing
        else Just (arcFrom s l)

data OpF n r
  = OpEmpty
  | OpSamp !InternalSamples
  | OpShift !Delta r
  | OpRepeat !Reps r
  | OpSlice !(Arc Time) r
  | OpConcat !(Seq r)
  | OpMerge !(Seq r)
  | OpRef !n
  deriving stock (Eq, Show, Functor, Foldable, Traversable)

type Op n = Fix (OpF n)

opRefs :: (Ord n) => Op n -> Set n
opRefs = cata $ \case
  OpRef n -> Set.singleton n
  op -> fold op

opAnnoRefs :: (Ord n) => Memo (OpF n) a -> Set n
opAnnoRefs = opRefs . memoFix

newtype Extent = Extent {unExtent :: Arc Time}
  deriving stock (Eq, Ord, Show)

mkExtent :: Arc Time -> Extent
mkExtent arc@(Arc s e) = Extent (if s >= e then Arc 0 0 else arc)

extentFromDelta :: Delta -> Extent
extentFromDelta = Extent . arcFrom 0

extentEmpty :: Extent
extentEmpty = Extent (Arc 0 0)

extentPosArc :: Extent -> Maybe (Arc Time)
extentPosArc (Extent (Arc s e)) =
  if e <= 0 || s >= e
    then Nothing
    else Just (Arc 0 e)

extentLen :: Extent -> Delta
extentLen = maybe 0 arcLen . extentPosArc

extentShift :: Extent -> Delta -> Extent
extentShift (Extent arc) c = Extent (if arcNull arc then arc else arcShift arc c)

extentRepeat :: Reps -> Extent -> Extent
extentRepeat n ext =
  if n <= 0
    then extentEmpty
    else maybe extentEmpty (Extent . arcRepeat n) (extentPosArc ext)

newtype ExtentMerge = ExtentMerge {unExtentMerge :: Extent}

instance Semigroup ExtentMerge where
  ExtentMerge (Extent a) <> ExtentMerge (Extent b) =
    ExtentMerge (Extent (arcUnion a b))

instance Monoid ExtentMerge where
  mempty = ExtentMerge extentEmpty

extentMerge :: (Foldable f) => f Extent -> Extent
extentMerge = unExtentMerge . foldMap ExtentMerge

newtype ExtentConcat = ExtentConcat {unExtentConcat :: Extent}

instance Semigroup ExtentConcat where
  ExtentConcat (Extent (Arc sa ea)) <> ExtentConcat (Extent (Arc sb eb)) =
    ExtentConcat (Extent (Arc 0 (ea - sa + eb - sb)))

instance Monoid ExtentConcat where
  mempty = ExtentConcat extentEmpty

extentConcat :: (Foldable f) => f Extent -> Extent
extentConcat = unExtentConcat . foldMap ExtentConcat

opInferExtentF :: (Monad m) => Rate -> (n -> m Extent) -> OpF n Extent -> m Extent
opInferExtentF rate onRef = \case
  OpEmpty -> pure extentEmpty
  OpSamp x ->
    let i = isampsLength x
    in  pure $
          if i == 0
            then extentEmpty
            else mkExtent (unquantize rate (Arc 0 i))
  OpRepeat n r -> pure (extentRepeat n r)
  OpSlice sarc _ -> pure (extentFromDelta (arcLen sarc))
  OpShift c r -> pure (extentShift r (negate c))
  OpConcat rs -> pure (extentConcat rs)
  OpMerge rs -> pure (extentMerge rs)
  OpRef n -> onRef n

opInferExtent :: (Monad m) => Rate -> (n -> m Extent) -> Op n -> m Extent
opInferExtent rate f = cataM (opInferExtentF rate f)

opInferExtentSingle :: Rate -> Op n -> Either n Extent
opInferExtentSingle rate = opInferExtent rate Left

opInferExtentTopo :: (Ord n) => Rate -> Map n (Op n) -> Either (SortErr n) (Map n (Either n Extent))
opInferExtentTopo rate m = topoEval opRefs m (opInferExtent rate)

opAnnoExtent :: (Monad m) => Rate -> (n -> m Extent) -> Op n -> m (Memo (OpF n) Extent)
opAnnoExtent rate f = mkMemoM (opInferExtentF rate f)

opAnnoExtentSingle :: Rate -> Op n -> Either n (Memo (OpF n) Extent)
opAnnoExtentSingle rate = opAnnoExtent rate Left

opAnnoExtentTopo :: (Ord n) => Rate -> Map n (Op n) -> Either (SortErr n) (Map n (Either n (Memo (OpF n) Extent)))
opAnnoExtentTopo rate m = topoAnnoM opRefs m (opInferExtentF rate)

newtype Samples = Samples {runSamples :: Arc Time -> InternalSamples}

arrEmpty :: Rate -> Arc Time -> InternalSamples
arrEmpty rate arc = isampsConstant (arcLen @ElemCount (quantize rate arc)) 0

orEmpty :: Rate -> Samples
orEmpty = Samples . arrEmpty

orSamp :: Rate -> InternalSamples -> Samples
orSamp rate x =
  let i = isampsLength x
  in  if i == 0
        then orEmpty rate
        else Samples $ \arc ->
          let Arc s e = quantize rate arc
              s' = max 0 s
              e' = min i e
              l = e - s
              l' = e' - s'
          in  if s' >= i || e' <= 0 || s' >= e'
                then isampsConstant l 0
                else
                  let preSamps = isampsConstant (s' - s) 0
                      postSamps = isampsConstant (e - e') 0
                      bodySamps = isampsTrim s' l' x
                      samps = [preSamps, bodySamps, postSamps]
                  in  isampsConcat samps

orRepeat :: Rate -> Reps -> Extent -> Samples -> Samples
orRepeat rate n rext rsamp =
  if n <= 0
    then Samples (arrEmpty rate)
    else case extentPosArc rext of
      Nothing -> Samples (arrEmpty rate)
      Just sarc -> Samples $ \arc ->
        let -- Get the total length of one repetition
            repLen = arcLen sarc
            -- Get the relative position within the total length
            relPos = arcStart arc
            -- Get the number of full repetitions before the start
            fullReps = floor (unTime relPos / unDelta repLen)
            -- Get the offset within a single repetition
            repOff = Time (unTime relPos - fromInteger fullReps * unDelta repLen)
            -- Get the length we need to generate
            genLen = arcLen arc
            -- Calculate how many full repetitions we need
            numReps = ceiling (unDelta genLen / unDelta repLen) + 1
            -- Get the minimal sub-arc we need to generate
            subArc = arcFrom repOff repLen
            -- Generate the base samples
            baseSamps = runSamples rsamp subArc
            -- Generate all repetitions
            allReps = isampsReplicate numReps baseSamps
            -- Trim to the exact length needed
            trimOff = ElemCount (unElemCount (arcLen @ElemCount (quantize rate (Arc 0 relPos))))
            trimLen = ElemCount (unElemCount (arcLen @ElemCount (quantize rate arc)))
        in isampsTrim trimOff trimLen allReps

orSlice :: Rate -> Arc Time -> Samples -> Samples
orSlice rate sarc rsamp =
  if arcNull sarc
    then Samples (arrEmpty rate)
    else Samples $ \arc ->
      case arcRelative arc sarc of
        Just (before, arc', after) ->
          let preSamps = arrEmpty rate (arcFrom (arcStart arc') (negate before))
              bodySamps = runSamples rsamp arc'
              postSamps = arrEmpty rate (arcFrom (arcEnd arc') after)
              samps = [preSamps, bodySamps, postSamps]
          in  isampsConcat samps
        Nothing -> arrEmpty rate arc

orShift :: Rate -> Delta -> Samples -> Samples
orShift rate c rsamp = Samples $ \arc ->
  runSamples rsamp (arcShift arc c)

-- let arr = runSamples rsamp (arcShift arc (negate c))
--     arrLen = isampsLength arr
--     len = arcLen @ElemCount (quantize rate arc)
-- in if c < 0 && arrLen < len
--   then
--     -- On negative shift, we need to front-pad the output
--     isampsConcat [isampsConstant (len - arrLen) 0, arr]
--   else arr

orConcat :: Rate -> Seq (Anno Extent Samples) -> Samples
orConcat rate rs =
  let gather (d, t, a) (Anno l g) =
        let q = extentLen l
            d' = d + q
            t' = addDelta t q
        in  (d', t', a :|> (d, Arc t t', g))
      (_, tot, subArcs) = foldl' gather (0, 0, Empty) rs
      whole = Arc 0 tot
  in  Samples $ \arc@(Arc s e) ->
        let elemLen = arcLen @ElemCount (quantize rate arc)
        in  case arcIntersect arc whole of
              Nothing -> isampsConstant elemLen 0
              Just filtArc@(Arc fs fe) ->
                let preSamps = arrEmpty rate (Arc s fs)
                    postSamps = arrEmpty rate (Arc fe e)
                    gen !samps = \case
                      Empty -> samps
                      (subDelta, subArc, subGen) :<| rest -> do
                        case arcOverlap filtArc subArc of
                          OverlapLt -> samps
                          OverlapOn _ overArc _ ->
                            let overArc' = arcShift overArc (negate subDelta)
                                subSamps = runSamples subGen overArc'
                            in  gen (samps :|> subSamps) rest
                          OverlapGt -> gen samps rest
                    genSamps = gen Empty subArcs
                in  isampsConcat (toList (preSamps :<| (genSamps :|> postSamps)))

orMerge :: [Samples] -> Samples
orMerge rsamps = Samples (\arc -> isampsMix (fmap (`runSamples` arc) rsamps))

opRender :: (Monad m) => Rate -> (n -> m Samples) -> Memo (OpF n) Extent -> m Samples
opRender rate onRef = goTop
 where
  goTop = memoRecallM go
  go = \case
    OpEmpty -> pure (orEmpty rate)
    OpSamp x -> pure (orSamp rate x)
    OpRepeat n r -> pure (orRepeat rate n (annoKey r) (annoVal r))
    OpSlice arc r -> pure (orSlice rate arc (annoVal r))
    OpShift c r -> pure (orShift rate c (annoVal r))
    OpConcat rs -> pure (orConcat rate rs)
    OpMerge rs -> pure (orMerge (fmap annoVal (toList rs)))
    OpRef n -> lift (onRef n)

opRenderTopo :: (Ord n) => Rate -> Map n (Memo (OpF n) Extent) -> Either (SortErr n) (Map n Samples)
opRenderTopo rate m = fmap (fmap runIdentity) (topoEval opAnnoRefs m (opRender rate))

opRenderSimple :: Rate -> Op n -> Either n InternalSamples
opRenderSimple rate op = do
  op' <- opAnnoExtentSingle rate op
  samps <- opRender rate Left op'
  pure (maybe isampsEmpty (runSamples samps) (extentPosArc (memoKey op')))

data View z = View
  { viewBounds :: !(Arc ElemCount)
  , viewArray :: !z
  }
  deriving stock (Eq, Ord, Show)

viewLen :: View z -> ElemCount
viewLen = arcLen . viewBounds

viewNull :: View z -> Bool
viewNull = arcNull . viewBounds

-- Return a new view with the start of the view bounds increased by the given amount.
-- Return empty if the resulting arc is null (zero-length).
viewSkip :: ElemCount -> View z -> Maybe (View z)
viewSkip skip (View bounds arr) = fmap (`View` arr) (arcSkip skip bounds)

-- Narrow to the relative sub-bounds or return empty.
viewNarrow :: Arc ElemCount -> View z -> Maybe (View z)
viewNarrow rel (View bounds arr) = fmap (`View` arr) (arcNarrow rel bounds)

type ArrayView a = View (PrimArray a)

avNew :: (Prim a) => PrimArray a -> ArrayView a
avNew arr = View (Arc 0 (ElemCount (sizeofPrimArray arr))) arr

type MutArrayView s a = View (MutablePrimArray s a)

type ParArrayView a = MutArrayView RealWorld a

type PrimMonadState s m = (PrimMonad m, PrimState m ~ s)

mavNew :: (PrimMonadState s m, Prim a) => MutablePrimArray s a -> m (MutArrayView s a)
mavNew arr = fmap (\len -> View (Arc 0 (ElemCount len)) arr) (getSizeofMutablePrimArray arr)

mavCopy :: (PrimMonadState s m, Prim a) => MutArrayView s a -> ArrayView a -> m ()
mavCopy (View darc@(Arc dstart _) darr) (View sarc@(Arc sstart _) sarr) = do
  let slen = arcLen sarc
      dlen = arcLen darc
      len = min slen dlen
  when
    (len > 0)
    (copyPrimArray darr (unElemCount dstart) sarr (unElemCount sstart) (unElemCount len))

newtype MutSamples = MutSamples {runMutSamples :: Arc Time -> Mutex (ParArrayView Int32) -> PrimPar ()}

runMutSamplesSimple :: Rate -> MutSamples -> Arc Time -> IO (PrimArray Int32)
runMutSamplesSimple rate samps arc = do
  let elemsLen = arcLen @ElemCount (quantize rate arc)
  arr <- zeroPrimArray (unElemCount elemsLen)
  mutView <- mavNew arr
  runPrimPar $ do
    bufVar <- newMutex mutView
    runMutSamples samps arc bufVar
  unsafeFreezePrimArray arr

ormEmpty :: MutSamples
ormEmpty = MutSamples (\_ _ -> pure ())

ormSamp :: Rate -> InternalSamples -> MutSamples
ormSamp rate x =
  let src = avNew (unInternalSamples x)
  in  if viewNull src
        then ormEmpty
        else MutSamples $ \arc bufVar -> void $ runMaybeT $ do
          let arc' = quantize rate arc
          src' <- maybe empty pure (viewNarrow arc' src)
          lift (withMutex bufVar (`mavCopy` src'))

opRenderMut
  :: forall e m n. (Monad m) => Rate -> (n -> ExceptT e m MutSamples) -> Memo (OpF n) Extent -> ExceptT e m MutSamples
opRenderMut rate onRef = goTop
 where
  goTop :: Memo (OpF n) Extent -> ExceptT e m MutSamples
  goTop = memoRecallM go
  go :: OpF n (Anno Extent MutSamples) -> ReaderT Extent (ExceptT e m) MutSamples
  go = \case
    OpEmpty -> pure ormEmpty
    OpSamp x -> pure (ormSamp rate x)
    OpRepeat _n _r -> error "TODO"
    OpSlice (Arc _ss _se) (Anno _rext _rsamp) -> error "TODO"
    OpShift _c _r -> error "TODO"
    OpConcat _rs -> error "TODO"
    OpMerge _rs -> error "TODO"
    OpRef n -> lift (onRef n)

opRenderMutSimple :: Rate -> Op n -> IO (Either n InternalSamples)
opRenderMutSimple rate op = runExceptT $ do
  op' <- either throwError pure (opAnnoExtentSingle rate op)
  samps <- opRenderMut rate throwError op'
  let marc = extentPosArc (memoKey op')
  maybe (pure isampsEmpty) (fmap InternalSamples . liftIO . runMutSamplesSimple rate samps) marc

-- opRenderMutSimple :: Rate -> Op n -> Either n (IO InternalSamples)
-- opRenderMutSimple rate op = do
--   op' <- opAnnoExtentSingle rate op
--   samps <- opRenderMut rate Left op'
--   pure $ case extentPosArc (memoKey op') of
--     Nothing -> pure isampsEmpty
--     Just whole -> do
--       let elemsLen = arcLen @ElemCount (quantize rate whole)
--       arr <- zeroPrimArray (unElemCount elemsLen)
--       runPrimPar (runMutSamples samps whole arr)
--       fmap InternalSamples (unsafeFreezePrimArray arr)

-- data OpEnv n = OpEnv
--   { oeDefs :: !(Map n (Memo (OpF n) Extent))
--   , oeFutVar :: !(MVar (Map n (IVar (PrimArray Int32))))
--   , oeOff :: !ElemCount
--   , oeBufVar :: !(MVar (MutablePrimArray RealWorld Int32))
--   }
--   deriving stock (Eq)

-- type OpM n = ReadPar (OpEnv n)

-- execOpM :: Map n (Memo (OpF n) Extent) -> ElemCount -> OpM n () -> IO (PrimArray Int32)
-- execOpM defs len act = do
--   buf <- newPrimArray (unElemCount len)
--   bufVar <- newMVar buf
--   futVar <- newMVar Map.empty
--   let env = OpEnv {oeDefs = defs, oeFutVar = futVar, oeOff = 0, oeBufVar = bufVar}
--   runReadPar act env
--   unsafeFreezePrimArray buf

-- handleOpSamp :: ElemCount -> ElemCount -> InternalSamples -> OpM n ()
-- handleOpSamp coff clen (InternalSamples src) = do
--   OpEnv _ _ off bufVar <- ask
--   liftIO $ withMVar bufVar $ \b -> copyPrimArray b (unElemCount off) src (unElemCount coff) (unElemCount clen)

-- handleOpReplicate :: (Ord n) => Int -> Op2Anno n -> OpM n ()
-- handleOpReplicate n r = do
--   OpEnv _ _ off bufVar <- ask
--   let MemoP rlen _ = r
--   goOpToWave r
--   liftIO $ withMVar bufVar $ \b -> do
--     for_ [1 .. n - 1] $ \i -> do
--       let off' = off + ElemCount i * rlen
--       copyMutablePrimArray b (unElemCount off') b (unElemCount off) (unElemCount rlen)

-- handleOpConcat :: (Ord n) => Seq (Op2Anno n) -> OpM n ()
-- handleOpConcat rs = do
--   off <- asks oeOff
--   parFor (InclusiveRange 0 (length rs - 1)) $ \i -> do
--     let r = Seq.index rs i
--         elemOff = sum (take i (map memoKey (toList rs)))
--         off' = off + elemOff
--     local (\env -> env {oeOff = off'}) (goOpToWave r)

-- handleOpMerge :: (Ord n) => Seq (Op2Anno n) -> OpM n ()
-- handleOpMerge rs = do
--   parFor (InclusiveRange 0 (length rs - 1)) $ \i -> do
--     let r = Seq.index rs i
--         rlen = memoKey r
--     tmp <- liftIO (zeroPrimArray (unElemCount rlen))
--     tmpVar <- liftIO (newMVar tmp)
--     local (\env -> env {oeOff = 0, oeBufVar = tmpVar}) (goOpToWave r)
--     OpEnv _ _ off bufVar <- ask
--     liftIO $ withMVar bufVar $ \b ->
--       mergeMutableIntoPrimArray (+) b (unElemCount off) tmp 0 (unElemCount rlen)

-- handleOpRef :: (Ord n) => ElemCount -> ElemCount -> n -> OpM n ()
-- handleOpRef _coff clen n = do
--   OpEnv defs futVar off bufVar <- ask
--   let def = defs Map.! n
--   ivar <- Par.new
--   xvar <- liftIO $ modifyMVar futVar $ \fut -> do
--     pure $ case Map.lookup n fut of
--       Nothing -> (Map.insert n ivar fut, Nothing)
--       Just var -> (fut, Just var)
--   val <- case xvar of
--     Nothing -> do
--       void $ Par.spawn_ $ Par.put_ ivar =<< liftIO (execOpM defs clen (goOpToWave def))
--       Par.get ivar
--     Just yvar -> Par.get yvar
--   liftIO $ withMVar bufVar $ \b ->
--     copyPrimArray b (unElemCount off) val 0 (unElemCount clen)

-- goOpToWave :: (Ord n) => Op2Anno n -> OpM n ()
-- goOpToWave (MemoP clen f) = case f of
--   Op2Empty -> pure ()
--   Op2Samp (Extent coff _) s -> handleOpSamp coff clen s
--   Op2Replicate n r -> handleOpReplicate n r
--   Op2Concat rs -> handleOpConcat rs
--   Op2Merge rs -> handleOpMerge rs
--   Op2Ref (Extent coff _) n -> handleOpRef coff clen n

-- data OpErr n
--   = OpErrInfer !(TopoErr n)
--   | OpErrAnno !(AnnoErr n)
--   | OpErrRoot !n
--   deriving stock (Eq, Ord, Show)

-- instance (Show n, Typeable n) => Exception (OpErr n)

-- opsToWave :: (Ord n) => Map n (Op n) -> n -> IO (Either (OpErr n) InternalSamples)
-- opsToWave defs n = runExceptT $ do
--   infs <- either (throwError . OpErrInfer) pure (opInferLenTopo defs)
--   anns <- either (throwError . OpErrAnno) pure (opAnnoLenTopo defs infs)
--   root <- maybe (throwError (OpErrRoot n)) pure (Map.lookup n anns)
--   arr <- liftIO $ execOpM anns (memoKey root) (goOpToWave root)
--   pure (InternalSamples arr)

-- opToWave :: (Ord n) => n -> Op n -> IO (Either (OpErr n) InternalSamples)
-- opToWave n op = opsToWave (Map.singleton n op) n
