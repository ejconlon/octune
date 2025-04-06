{-# OPTIONS_GHC -Wno-identities #-}

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

-- | A utility function for debugging that traces a list of strings.
traceAll :: [[String]] -> a -> a
traceAll xs = if True then id else trace ("\n<==\n" ++ unlines (fmap (("* " ++) . unwords) xs) ++ "==>\n")

-- Bowtie utils

-- | A utility function for recalling memoized values in a monadic context.
memoRecallM :: (Monad m, Traversable f) => (f (Anno k v) -> ReaderT k m v) -> Memo f k -> m v
memoRecallM f = go
 where
  go (MemoP k fm) = traverse (\m -> fmap (Anno (memoKey m)) (go m)) fm >>= \fa -> runReaderT (f fa) k

-- Prim adapters

-- | Create a new primitive array filled with zeros.
zeroPrimArray :: (PrimMonad m, Prim a, Num a) => Int -> m (MutablePrimArray (PrimState m) a)
zeroPrimArray n = do
  marr <- newPrimArray n
  setPrimArray marr 0 n 0
  pure marr

-- | Replicate a primitive array a given number of times.
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

-- | Concatenate multiple primitive arrays into one.
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

-- | Merge one primitive array into another using a combining function.
mergeIntoPrimArray
  :: (PrimMonad m, Prim a) => (a -> a -> a) -> MutablePrimArray (PrimState m) a -> Int -> PrimArray a -> Int -> Int -> m ()
mergeIntoPrimArray f darr doff sarr soff slen =
  for_ [0 .. slen - 1] $ \pos -> do
    let dpos = doff + pos
        spos = soff + pos
    val0 <- readPrimArray darr dpos
    let val1 = indexPrimArray sarr spos
    writePrimArray darr dpos (f val0 val1)

-- | Merge one mutable primitive array into another using a combining function.
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

-- | Merge multiple primitive arrays using a combining function.
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

-- | Convert a byte count to an element count for Int32 arrays. (Mono only)
i32ByteToElemCount :: ByteCount -> ElemCount
i32ByteToElemCount = ElemCount . (4 *) . unByteCount

-- | Convert an element count to a byte count for Int32 arrays. (Mono only)
i32ElemToByteCount :: ElemCount -> ByteCount
i32ElemToByteCount = ByteCount . (`div` 4) . unElemCount

newtype InternalSamples = InternalSamples {unInternalSamples :: PrimArray Int32}
  deriving stock (Eq, Show)
  deriving newtype (NFData)

-- | Create an empty samples array.
isampsEmpty :: InternalSamples
isampsEmpty = InternalSamples emptyPrimArray

-- | Check if an samples array is empty.
isampsNull :: InternalSamples -> Bool
isampsNull = (0 ==) . isampsBytes

-- | Get the length of an samples array in elements.
isampsLength :: InternalSamples -> ElemCount
isampsLength = ElemCount . sizeofPrimArray . unInternalSamples

-- | Get the length of an samples array in bytes.
isampsBytes :: InternalSamples -> ByteCount
isampsBytes = i32ElemToByteCount . isampsLength

-- | Get an element from an samples array.
isampsIndex :: InternalSamples -> ElemCount -> Int32
isampsIndex s = indexPrimArray (unInternalSamples s) . unElemCount

-- | Create an samples array filled with a constant value.
isampsConstant :: ElemCount -> Int32 -> InternalSamples
isampsConstant len = InternalSamples . replicatePrimArray (unElemCount len)

-- | Replicate an samples array a given number of times.
isampsReplicate :: Int -> InternalSamples -> InternalSamples
isampsReplicate n = InternalSamples . replicateWholePrimArray n . unInternalSamples

-- | Create an samples array from a list.
isampsFromList :: [Int32] -> InternalSamples
isampsFromList = InternalSamples . primArrayFromList

-- | Concatenate multiple samples arrays.
isampsConcat :: [InternalSamples] -> InternalSamples
isampsConcat = InternalSamples . concatPrimArray . fmap unInternalSamples

-- | Mix multiple internal arrays by adding their values.
isampsMix :: [InternalSamples] -> InternalSamples
isampsMix = InternalSamples . mergePrimArray (+) . fmap unInternalSamples

-- | Extract a sub-range from an samples array.
isampsTrim :: ElemCount -> ElemCount -> InternalSamples -> InternalSamples
isampsTrim off len s = InternalSamples (clonePrimArray (unInternalSamples s) (unElemCount off) (unElemCount len))

-- | Map a function over an samples array.
isampsMap :: (Int32 -> Int32) -> InternalSamples -> InternalSamples
isampsMap f = InternalSamples . mapPrimArray f . unInternalSamples

-- | Fill a sub-range of an samples array with a constant value.
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

-- | Convert samples to WAV format.
isampsToWave :: InternalSamples -> WAVESamples
isampsToWave = WAVESamples . QuietLiftedArray . LiftedPrimArray . (\(PrimArray x) -> ByteArray x) . unInternalSamples

-- | Convert WAV format to samples.
isampsFromWave :: WAVESamples -> InternalSamples
isampsFromWave = InternalSamples . (\(ByteArray x) -> PrimArray x) . unLiftedPrimArray . unQuietLiftedArray . unWAVESamples

-- | A stream of samples that can be repeated.
data SampleStream = SampleStream
  { ssFixed :: !InternalSamples
  -- ^ The fixed portion of the stream
  , ssRepeated :: !InternalSamples
  -- ^ The repeated portion of the stream
  }
  deriving stock (Eq, Show)

-- | Run a sample stream at a given position.
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

-- | Convert a sample stream to samples.
streamToIsamps :: ElemCount -> ElemCount -> SampleStream -> InternalSamples
streamToIsamps off len t =
  InternalSamples (generatePrimArray (unElemCount len) (streamRun t . (off +) . ElemCount))

-- | Assert that a WAV file is mono.
assertMono :: WAVEHeader -> IO ()
assertMono hdr = unless (waveNumChannels hdr == 1) (fail "sample wav must be mono")

-- | Read samples from a WAV file.
readSamples :: WAVE -> IO InternalSamples
readSamples (WAVE hdr samps) = do
  assertMono hdr
  pure (isampsFromWave samps)

-- | Write samples to a WAV file.
writeSamples :: WAVEHeader -> InternalSamples -> IO WAVE
writeSamples hdr isamps = do
  assertMono hdr
  pure (WAVE hdr (isampsToWave isamps))

-- | Load samples from a file.
loadSamples :: FilePath -> IO InternalSamples
loadSamples = getWAVEFile >=> readSamples

-- | Load samples from resources, falling back to filesystem if needed.
loadDataSamples :: String -> InternalSamples
loadDataSamples name =
  -- Default to resources but fall back to filesystem for ghci
  let part = "data/" ++ name ++ ".wav"
  in  unsafePerformIO $ do
        resPath <- getDataFileName part
        resExists <- doesFileExist resPath
        loadSamples (if resExists then resPath else part)

-- | Pre-loaded snare drum samples.
snareSamples :: InternalSamples
snareSamples = loadDataSamples "snare"

-- | Pre-loaded clap samples.
clapSamples :: InternalSamples
clapSamples = loadDataSamples "clap"

-- | Convert internal samples to a WAV file.
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

-- | Dump all samples to WAV files.
dumpAllSamples :: IO ()
dumpAllSamples = do
  let items = [] :: [(String, InternalSamples)]
  for_ items $ \(name, samps) -> dumpSamples samps >>= putWAVEFile ("data/" ++ name ++ ".wav")

-- | A type class for measuring distances between points and adding deltas to points.
class (Ord t, Ord d, Num d) => Measure t d | t -> d where
  -- | Calculate the distance between two points.
  measureDelta :: t -> t -> d

  -- | Add a delta to a point.
  addDelta :: t -> d -> t

instance Measure ElemCount ElemCount where
  measureDelta c1 c2 = c2 - c1
  addDelta c d = c + d

-- | A type class for converting between continuous and discrete representations of time.
class (Ord t, Ord r) => Quantize t r | t -> r where
  -- | Convert a continuous time arc to a discrete arc.
  quantize :: (Integral u) => r -> Arc t -> Arc u

  -- | Convert a discrete arc back to a continuous time arc.
  unquantize :: (Integral u) => r -> Arc u -> Arc t

-- | A continuous time value represented as a rational number.
newtype Time = Time {unTime :: Rational}
  deriving stock (Eq, Show)
  deriving newtype (Num, Ord, Enum, Real, Fractional, RealFrac)

-- | A time delta value represented as a rational number.
newtype Delta = Delta {unDelta :: Rational}
  deriving stock (Eq, Show)
  deriving newtype (Num, Ord, Enum, Real, Fractional, RealFrac)

-- deriving (Semigroup, Monoid) via (Sum Rational)

instance Measure Time Delta where
  measureDelta (Time t1) (Time t2) = Delta (t2 - t1)
  addDelta (Time t) (Delta d) = Time (t + d)

-- | A rate value representing "things per unit time" as a rational number.
newtype Rate = Rate {unRate :: Rational}
  deriving stock (Eq, Show)
  deriving newtype (Num, Ord, Enum, Real, Fractional, RealFrac)

instance Quantize Time Rate where
  quantize (Rate r) (Arc (Time s) (Time e)) = Arc (truncate (s * r)) (ceiling (e * r))
  unquantize (Rate r) (Arc s e) = Arc (Time (fromIntegral s / r)) (Time (fromIntegral e / r))

-- | A number of repetitions represented as a rational number.
newtype Reps = Reps {unReps :: Rational}
  deriving stock (Eq, Show)
  deriving newtype (Num, Ord, Enum, Real, Fractional, RealFrac)

mulReps :: (Fractional d) => Reps -> d -> d
mulReps (Reps n) d = fromRational n * d

-- | A half-open interval [start, end) representing a range of values.
-- Invariants:
--   * For any Arc t, arcStart t <= arcEnd t
--   * An arc is empty (arcNull) if and only if arcStart t == arcEnd t
data Arc t = Arc
  { arcStart :: !t
  -- ^ The start of the interval (inclusive)
  , arcEnd :: !t
  -- ^ The end of the interval (exclusive)
  }
  deriving stock (Eq, Ord, Show)

-- | An empty arc starting and ending at 0.
arcEmpty :: (Num t) => Arc t
arcEmpty = Arc 0 0

-- | Compute the union of two arcs.
arcUnion :: (Ord t) => Arc t -> Arc t -> Arc t
arcUnion (Arc s1 e1) (Arc s2 e2) =
  let s3 = min s1 s2
      e3 = max e1 e2
  in  Arc s3 e3

-- | Compute the intersection of two arcs, if any.
arcIntersect :: (Measure t d) => Arc t -> Arc t -> Maybe (Arc t)
arcIntersect (Arc s1 e1) (Arc s2 e2) =
  let s3 = max s1 s2
      e3 = min e1 e2
  in  if s2 >= e1 || s1 >= e2 || s3 >= e3 then Nothing else Just (Arc s3 e3)

-- | Calculate the length of an arc.
arcLen :: (Measure t d) => Arc t -> d
arcLen (Arc s e) = measureDelta s e

-- | Create an arc from a start point and a length.
arcFrom :: (Measure t d) => t -> d -> Arc t
arcFrom t d =
  let u = addDelta t d
  in  if t <= u then Arc t u else Arc u t

-- | Check if an arc is empty (has zero length).
arcNull :: (Measure t d) => Arc t -> Bool
arcNull = (0 ==) . arcLen

-- | Shift an arc by a delta.
arcShift :: (Measure t d) => Arc t -> d -> Arc t
arcShift (Arc s e) d = Arc (addDelta s d) (addDelta e d)

-- | Repeat an arc a given number of times.
arcRepeat :: (Measure t d, Fractional d) => Reps -> Arc t -> Arc t
arcRepeat n (Arc s e) = Arc s (addDelta s (mulReps n (measureDelta s e)))

-- | Calculate how many times a delta fits into an arc.
-- Returns a tuple of:
--   * The amount of negative time before the first repetition
--   * The index of the first repetition that overlaps the arc
--   * The index of the last repetition that overlaps the arc
arcDeltaCover :: (Measure t d, Real t, RealFrac d) => d -> Arc t -> Maybe (d, Integer, Integer)
arcDeltaCover d (Arc s e) =
  if s >= e || d <= 0
    then Nothing
    else
      let
        -- Calculate how much of the arc is in negative time
        negDelta = if s < 0 then measureDelta s 0 else 0
        -- Find the greatest n where n*d ≤ s
        firstN = if s < 0 then 0 else floor (toRational s / toRational d)
        -- Find the smallest n where n*d ≥ e
        lastN = ceiling (toRational e / toRational d)
      in
        Just (negDelta, firstN, lastN)

-- | Calculate how many times a delta fits into an arc, with a maximum number of repetitions.
-- Returns a tuple of:
--   * The amount of negative time before the first repetition
--   * The index of the first repetition that overlaps the arc
--   * The index of the last repetition that overlaps the arc (capped by maxReps)
--   * The amount of time beyond the last repetition that overlaps the arc
arcDeltaCoverMax :: (Measure t d, Real t, RealFrac d) => d -> Reps -> Arc t -> Maybe (d, Integer, Integer, d)
arcDeltaCoverMax d maxReps arc =
  if maxReps <= 0
    then Nothing
    else do
      let intMaxReps = ceiling maxReps
      (negDelta, firstN, lastN) <- arcDeltaCover d arc
      let (cappedN, beyondMax) =
            if lastN >= intMaxReps
              then (intMaxReps, measureDelta (addDelta 0 (fromRational (unReps maxReps) * d)) (arcEnd arc))
              else (lastN, 0)
      pure (negDelta, firstN, cappedN, beyondMax)

-- | Describes how two arcs overlap.
data Overlap t d
  = -- | The first arc is entirely before the second arc
    OverlapLt
  | -- | The arcs overlap, with:
    --   * The amount of time before the overlap
    --   * The overlapping portion
    --   * The amount of time after the overlap
    OverlapOn !d !(Arc t) !d
  | -- | The first arc is entirely after the second arc
    OverlapGt
  deriving stock (Eq, Ord, Show)

-- | Calculate how two arcs overlap.
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

-- | Calculate the relative position of one arc within another.
-- Returns a tuple of:
--   * The amount of time before the first arc that's in the second arc
--   * The overlapping portion of the arcs
--   * The amount of time after the first arc that's in the second arc
arcRelative :: (Measure t d, Num t) => Arc t -> Arc t -> Maybe (d, Arc t, d)
arcRelative (Arc ns ne) (Arc hs he) =
  let s = max hs (addDelta hs (measureDelta 0 ns))
      e = min he (addDelta hs (measureDelta 0 ne))
  in  if s >= e
        then Nothing
        else
          Just
            ( max 0 (measureDelta ns (addDelta 0 (measureDelta hs s)))
            , Arc s e
            , max 0 (measureDelta (addDelta 0 (measureDelta hs e)) ne)
            )

-- | Skip a given amount from the start of an arc.
arcSkip :: (Measure t d) => d -> Arc t -> Maybe (Arc t)
arcSkip d (Arc bs be) =
  let s = addDelta bs d
  in  if s >= be
        then Nothing
        else Just (Arc s be)

-- | Narrow an arc to a relative sub-range.
arcNarrow :: (Measure t d, Num t) => Arc t -> Arc t -> Maybe (Arc t)
arcNarrow rel@(Arc rs _) base@(Arc bs _) =
  let s = bs + rs
      l = min (arcLen rel) (arcLen base)
  in  if l <= 0
        then Nothing
        else Just (arcFrom s l)

-- | A type representing audio operations.
data OpF n r
  = -- | An empty operation that produces no sound.
    OpEmpty
  | -- | A constant sample value.
    OpSamp !InternalSamples
  | -- | Shift an operation in time.
    OpShift !Delta r
  | -- | Repeat an operation a number of times.
    OpRepeat !Reps r
  | -- | Slice a portion of an operation.
    OpSlice !(Arc Time) r
  | -- | Concatenate multiple operations in sequence.
    OpConcat !(Seq r)
  | -- | Merge multiple operations by mixing their samples.
    OpMerge !(Seq r)
  | -- | Reference another operation by name.
    OpRef !n
  deriving stock (Eq, Show, Functor, Foldable, Traversable)

-- | A fixed point of the OpF functor.
type Op n = Fix (OpF n)

-- | Find all references in an operation.
opRefs :: (Ord n) => Op n -> Set n
opRefs = cata $ \case
  OpRef n -> Set.singleton n
  op -> fold op

-- | Find all references in an annotated operation.
opAnnoRefs :: (Ord n) => Memo (OpF n) a -> Set n
opAnnoRefs = opRefs . memoFix

-- | A time extent representing the duration of an operation.
newtype Extent = Extent {unExtent :: Arc Time}
  deriving stock (Eq, Ord, Show)

-- | Create an extent from an arc, ensuring it's valid.
mkExtent :: Arc Time -> Extent
mkExtent arc@(Arc s e) = Extent (if s >= e then Arc 0 0 else arc)

-- | Create an extent from a delta.
extentFromDelta :: Delta -> Extent
extentFromDelta = Extent . arcFrom 0

-- | An empty extent.
extentEmpty :: Extent
extentEmpty = Extent (Arc 0 0)

-- | Check if an extent is empty.
extentNull :: Extent -> Bool
extentNull = arcNull . unExtent

-- | Get the positive portion of an extent as an arc.
extentPosArc :: Extent -> Maybe (Arc Time)
extentPosArc (Extent (Arc s e)) =
  if e <= 0 || s >= e
    then Nothing
    else Just (Arc 0 e)

-- | Get the length of an extent.
extentLen :: Extent -> Delta
extentLen = maybe 0 arcLen . extentPosArc

-- | Shift an extent by a delta.
extentShift :: Extent -> Delta -> Extent
extentShift (Extent arc) c = Extent (if arcNull arc then arc else arcShift arc c)

-- | Repeat an extent a number of times.
extentRepeat :: Reps -> Extent -> Extent
extentRepeat n ext =
  if n <= 0
    then extentEmpty
    else maybe extentEmpty (Extent . arcRepeat n) (extentPosArc ext)

-- | A newtype for merging extents.
newtype ExtentMerge = ExtentMerge {unExtentMerge :: Extent}

instance Semigroup ExtentMerge where
  ExtentMerge (Extent (Arc sa ea)) <> ExtentMerge (Extent (Arc sb eb)) =
    ExtentMerge (Extent (Arc 0 (max (ea - sa) (eb - sb))))

instance Monoid ExtentMerge where
  mempty = ExtentMerge extentEmpty

-- | Merge multiple extents, taking the maximum duration.
extentMerge :: (Foldable f) => f Extent -> Extent
extentMerge = unExtentMerge . foldMap ExtentMerge

-- | A newtype for concatenating extents.
newtype ExtentConcat = ExtentConcat {unExtentConcat :: Extent}

instance Semigroup ExtentConcat where
  ExtentConcat (Extent (Arc sa ea)) <> ExtentConcat (Extent (Arc sb eb)) =
    ExtentConcat (Extent (Arc 0 (ea - sa + eb - sb)))

instance Monoid ExtentConcat where
  mempty = ExtentConcat extentEmpty

-- | Concatenate multiple extents, summing their durations.
extentConcat :: (Foldable f) => f Extent -> Extent
extentConcat = unExtentConcat . foldMap ExtentConcat

-- | Infer the extent of an operation.
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

-- | Infer the extent of an operation.
opInferExtent :: (Monad m) => Rate -> (n -> m Extent) -> Op n -> m Extent
opInferExtent rate f = cataM (opInferExtentF rate f)

-- | Infer the extent of an operation, handling references with Left.
opInferExtentSingle :: Rate -> Op n -> Either n Extent
opInferExtentSingle rate = opInferExtent rate Left

-- | Infer the extent of multiple operations in topological order.
opInferExtentTopo :: (Ord n) => Rate -> Map n (Op n) -> Either (SortErr n) (Map n (Either n Extent))
opInferExtentTopo rate m = topoEval opRefs m (opInferExtent rate)

-- | Annotate an operation with its extent.
opAnnoExtent :: (Monad m) => Rate -> (n -> m Extent) -> Op n -> m (Memo (OpF n) Extent)
opAnnoExtent rate f = mkMemoM (opInferExtentF rate f)

-- | Annotate an operation with its extent, handling references with Left.
opAnnoExtentSingle :: Rate -> Op n -> Either n (Memo (OpF n) Extent)
opAnnoExtentSingle rate = opAnnoExtent rate Left

-- | Annotate multiple operations with their extents in topological order.
opAnnoExtentTopo :: (Ord n) => Rate -> Map n (Op n) -> Either (SortErr n) (Map n (Either n (Memo (OpF n) Extent)))
opAnnoExtentTopo rate m = topoAnnoM opRefs m (opInferExtentF rate)

-- | A type representing audio samples.
newtype Samples = Samples {runSamples :: Arc Time -> InternalSamples}

-- | Create empty samples for a given arc.
arrEmpty :: Rate -> Arc Time -> InternalSamples
arrEmpty rate arc = isampsConstant (arcLen @ElemCount (quantize rate arc)) 0

-- | Create an empty sample generator.
orEmpty :: Rate -> Samples
orEmpty = Samples . arrEmpty

-- | Create a sample generator from constant samples.
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

-- | Create a sample generator that repeats another generator.
orRepeat :: Rate -> Reps -> Extent -> Samples -> Samples
orRepeat rate n rext rsamp =
  if n <= 0
    then Samples (arrEmpty rate)
    else case extentPosArc rext of
      Nothing -> Samples (arrEmpty rate)
      Just sarc ->
        let repLen = arcLen sarc
        in  Samples $ \arc ->
              case arcDeltaCoverMax repLen n arc of
                Nothing -> arrEmpty rate arc
                Just (negDelta, firstN, cappedN, beyondMax) ->
                  let
                    renderRep rep =
                      let
                        repDelta = Delta (fromInteger rep * unDelta repLen)
                        repArc = Arc (addDelta (arcStart sarc) repDelta) (addDelta (arcEnd sarc) repDelta)
                      in
                        case arcIntersect arc repArc of
                          Nothing -> isampsEmpty
                          Just subArc ->
                            let relArc = Arc (addDelta (arcStart subArc) (negate repDelta)) (addDelta (arcEnd subArc) (negate repDelta))
                            in  runSamples rsamp relArc
                    allSamps =
                      [ arrEmpty rate (Arc (arcStart arc) (addDelta (arcStart arc) negDelta))
                      ]
                        ++ map renderRep [firstN .. cappedN - 1]
                        ++ [ arrEmpty rate (Arc (addDelta (arcEnd arc) (negate beyondMax)) (arcEnd arc))
                           ]
                  in
                    isampsConcat allSamps

-- | Create a sample generator that slices another generator.
orSlice :: Rate -> Arc Time -> Samples -> Samples
orSlice rate sarc rsamp =
  if arcNull sarc
    then Samples (arrEmpty rate)
    else Samples $ \arc ->
      case arcRelative arc sarc of
        Just (before, arc', after) ->
          let preSamps = if before <= 0 then isampsEmpty else arrEmpty rate (Arc (arcStart arc) (arcStart arc'))
              bodySamps = runSamples rsamp arc'
              postSamps = if after <= 0 then isampsEmpty else arrEmpty rate (Arc (arcEnd arc') (arcEnd arc))
              samps = [preSamps, bodySamps, postSamps]
          in  isampsConcat samps
        Nothing -> arrEmpty rate arc

-- | Create a sample generator that shifts another generator in time.
orShift :: Rate -> Delta -> Samples -> Samples
orShift _rate c rsamp = Samples $ \arc ->
  runSamples rsamp (arcShift arc c)

-- | Create a sample generator that concatenates multiple generators.
orConcat :: Rate -> Seq (Anno Extent Samples) -> Samples
orConcat rate rs =
  let gather dta@(d, t, a) (Anno l g) =
        let q = extentLen l
        in  if q <= 0
              then dta
              else
                let d' = d + q
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

-- | Create a sample generator that merges multiple generators by mixing their samples.
orMerge :: [Samples] -> Samples
orMerge rsamps = Samples (\arc -> isampsMix (fmap (`runSamples` arc) rsamps))

-- | Render an operation to samples.
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

-- | Render multiple operations to samples in topological order.
opRenderTopo :: (Ord n) => Rate -> Map n (Memo (OpF n) Extent) -> Either (SortErr n) (Map n Samples)
opRenderTopo rate m = fmap (fmap runIdentity) (topoEval opAnnoRefs m (opRender rate))

-- | Render an operation to samples, handling references with Left.
opRenderSimple :: Rate -> Op n -> Either n InternalSamples
opRenderSimple rate op = do
  op' <- opAnnoExtentSingle rate op
  samps <- opRender rate Left op'
  pure (maybe isampsEmpty (runSamples samps) (extentPosArc (memoKey op')))

-- | A view of an array with bounds.
data View z = View
  { viewBounds :: !(Arc ElemCount)
  -- ^ The bounds of the view
  , viewArray :: !z
  -- ^ The array being viewed
  }
  deriving stock (Eq, Ord, Show)

-- | Get the length of a view.
viewLen :: View z -> ElemCount
viewLen = arcLen . viewBounds

-- | Check if a view is empty.
viewNull :: View z -> Bool
viewNull = arcNull . viewBounds

-- | Skip a number of elements from the start of a view.
viewSkip :: ElemCount -> View z -> Maybe (View z)
viewSkip skip (View bounds arr) = fmap (`View` arr) (arcSkip skip bounds)

-- | Narrow a view to a relative sub-range.
viewNarrow :: Arc ElemCount -> View z -> Maybe (View z)
viewNarrow rel (View bounds arr) = fmap (`View` arr) (arcNarrow rel bounds)

-- | A view of a primitive array.
type ArrayView a = View (PrimArray a)

-- | Create a view of a primitive array.
avNew :: (Prim a) => PrimArray a -> ArrayView a
avNew arr = View (Arc 0 (ElemCount (sizeofPrimArray arr))) arr

-- | A view of a mutable primitive array.
type MutArrayView s a = View (MutablePrimArray s a)

-- | A view of a mutable primitive array in the real world.
type ParArrayView a = MutArrayView RealWorld a

-- | A constraint for monads that can work with primitive state.
type PrimMonadState s m = (PrimMonad m, PrimState m ~ s)

-- | Create a view of a mutable primitive array.
mavNew :: (PrimMonadState s m, Prim a) => MutablePrimArray s a -> m (MutArrayView s a)
mavNew arr = fmap (\len -> View (Arc 0 (ElemCount len)) arr) (getSizeofMutablePrimArray arr)

-- | Copy from an array view to a mutable array view.
mavCopy :: (PrimMonadState s m, Prim a) => MutArrayView s a -> ArrayView a -> m ()
mavCopy (View darc@(Arc dstart _) darr) (View sarc@(Arc sstart _) sarr) = do
  let slen = arcLen sarc
      dlen = arcLen darc
      len = min slen dlen
  when
    (len > 0)
    (copyPrimArray darr (unElemCount dstart) sarr (unElemCount sstart) (unElemCount len))

-- | A type representing mutable samples.
newtype MutSamples = MutSamples {runMutSamples :: Arc Time -> Mutex (ParArrayView Int32) -> PrimPar ()}

-- | Run mutable samples to produce a primitive array.
runMutSamplesSimple :: Rate -> MutSamples -> Arc Time -> IO (PrimArray Int32)
runMutSamplesSimple rate samps arc = do
  let elemsLen = arcLen @ElemCount (quantize rate arc)
  arr <- zeroPrimArray (unElemCount elemsLen)
  mutView <- mavNew arr
  runPrimPar $ do
    bufVar <- newMutex mutView
    runMutSamples samps arc bufVar
  unsafeFreezePrimArray arr

-- | Create empty mutable samples.
ormEmpty :: MutSamples
ormEmpty = MutSamples (\_ _ -> pure ())

-- | Create mutable samples from constant samples.
ormSamp :: Rate -> InternalSamples -> MutSamples
ormSamp rate x =
  let src = avNew (unInternalSamples x)
  in  if viewNull src
        then ormEmpty
        else MutSamples $ \arc bufVar -> void $ runMaybeT $ do
          let arc' = quantize rate arc
          src' <- maybe empty pure (viewNarrow arc' src)
          lift (withMutex bufVar (`mavCopy` src'))

-- | Render an operation to mutable samples.
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

-- | Render an operation to mutable samples, handling references with Left.
opRenderMutSimple :: Rate -> Op n -> IO (Either n InternalSamples)
opRenderMutSimple rate op = runExceptT $ do
  op' <- either throwError pure (opAnnoExtentSingle rate op)
  samps <- opRenderMut rate throwError op'
  let marc = extentPosArc (memoKey op')
  maybe (pure isampsEmpty) (fmap InternalSamples . liftIO . runMutSamplesSimple rate samps) marc
