{-# OPTIONS_GHC -Wno-identities #-}

module Minipat.Octune.Time where

import Data.Maybe (fromMaybe)

-- | A type class for measuring distances between points and adding deltas to points.
class (Ord t, Ord d, Num d) => Measure t d | t -> d where
  -- | Calculate the distance between two points.
  measureDelta :: t -> t -> d

  -- | Add a delta to a point.
  addDelta :: t -> d -> t

-- | A continuous time value represented as a rational number.
newtype Time = Time {unTime :: Rational}
  deriving stock (Eq, Show)
  deriving newtype (Num, Ord, Enum, Real, Fractional, RealFrac)

-- | A time delta value represented as a rational number.
newtype Delta = Delta {unDelta :: Rational}
  deriving stock (Eq, Show)
  deriving newtype (Num, Ord, Enum, Real, Fractional, RealFrac)

instance Measure Time Delta where
  measureDelta (Time t1) (Time t2) = Delta (t2 - t1)
  addDelta (Time t) (Delta d) = Time (t + d)

-- | A quantized time value represented as an integer.
newtype QTime = QTime {unQTime :: Integer}
  deriving stock (Eq, Show)
  deriving newtype (Num, Ord, Enum, Real, Integral)

-- | A quantized time delta value represented as an integer.
newtype QDelta = QDelta {unQDelta :: Integer}
  deriving stock (Eq, Show)
  deriving newtype (Num, Ord, Enum, Real, Integral)

instance Measure QTime QDelta where
  measureDelta (QTime q1) (QTime q2) = QDelta (q2 - q1)
  addDelta (QTime q) (QDelta d) = QTime (q + d)

-- | A rate value representing "things per unit time" as a rational number.
newtype Rate = Rate {unRate :: Rational}
  deriving stock (Eq, Show)
  deriving newtype (Num, Ord, Enum, Real, Fractional, RealFrac)

-- | Convert a continuous time arc to a discrete arc.
-- Note that it is not possible to meaningfully convert
-- individual time points back and forth because we
-- choose to make delta quantization shift-invariant.
quantizeArc :: Rate -> Arc Time -> Arc QTime
quantizeArc (Rate r) (Arc (Time s) (Time e)) =
  let qstart = floor (s * r)
  in  if s == e
        then Arc qstart qstart
        else
          let qlen = ceiling ((e - s) * r)
              qend = qstart + qlen
          in  Arc qstart qend

-- | Convert a continuous time delta to a quantized delta.
quantizeDelta :: Rate -> Delta -> QDelta
quantizeDelta (Rate r) (Delta d) = QDelta (ceiling (d * r))

-- | Convert a discrete arc back to a continuous time arc.
unquantizeArc :: Rate -> Arc QTime -> Arc Time
unquantizeArc (Rate r) (Arc (QTime s) (QTime e)) = Arc (Time (fromIntegral s / r)) (Time (fromIntegral e / r))

-- | Convert a quantized delta to a continuous time delta.
unquantizeDelta :: Rate -> QDelta -> Delta
unquantizeDelta (Rate r) (QDelta d) = Delta (fromIntegral d / r)

-- | A number of repetitions represented as a rational number.
newtype Reps = Reps {unReps :: Rational}
  deriving stock (Eq, Show)
  deriving newtype (Num, Ord, Enum, Real, Fractional, RealFrac)

-- | Multiply a number of repetitions by a delta.
mulReps :: Reps -> Delta -> Delta
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
arcRepeat :: Reps -> Arc Time -> Arc Time
arcRepeat n (Arc s e) = Arc s (addDelta s (mulReps n (measureDelta s e)))

-- | Calculate how many times a delta fits into an arc.
-- Returns a tuple of:
--   * The amount of negative time before the first repetition (always non-negative)
--   * The index of the first repetition that overlaps the arc (always non-negative)
--   * The index of the last repetition that overlaps the arc (always non-negative)
arcDeltaCover :: (Measure t d, Real t, RealFrac d) => d -> Arc t -> Maybe (d, Integer, Integer)
arcDeltaCover d (Arc s e) =
  if s >= e || d <= 0 || e <= 0
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
--   * The amount of negative time before the first repetition (always non-negative)
--   * The index of the first repetition that overlaps the arc
--   * The index of the last repetition that overlaps the arc (capped by maxReps)
--   * The amount of time beyond the last repetition that overlaps the arc (always non-negative)
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
-- The returned time measurements (before and after) are always non-negative.
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
-- Note that the first arc is _relative_ to the second, meaning that you
-- can interpret the first arc's components as 'Arc offset (offset + length)'.
-- Example: 'arcRelative (Arc 1 2) (Arc 10 15) == Just (0, Arc 10 13, 0)'.
-- Example: 'arcRelative (Arc (-1) 8) (Arc 10 15) == Just (1, Arc 10 15, 2)'.
-- Returns Just a tuple of:
--   * The amount of time before the second arc that's in the first arc (always non-negative)
--   * The relevant portion of the second arc.
--   * The amount of time after the second arc that's in the first arc (always non-negative)
-- Or Nothing if the resulting arc is empty.
-- Invariants:
--   * The returned arc is contained within the second arc.
--   * The sum of the returned deltas and arc length is equal to the length of the first arc.
arcRelative :: (Measure t d, Num t) => Arc t -> Arc t -> Maybe (d, Arc t, d)
arcRelative (Arc off offLen) (Arc s0 e0) =
  let s1 = s0 + off
      e1 = s0 + offLen
      s = max s0 s1
      e = min e0 e1
  in  if s >= e
        then Nothing
        else Just (measureDelta s1 s, Arc s e, measureDelta e e1)

-- | Skip a given amount from the start of an arc.
arcSkip :: (Measure t d) => d -> Arc t -> Maybe (Arc t)
arcSkip d (Arc bs be) =
  let s = addDelta bs d
  in  if s >= be
        then Nothing
        else Just (Arc s be)

-- | Narrow an arc to a relative sub-range.
arcNarrow :: (Measure t d, Num t) => Arc t -> Arc t -> Maybe (Arc t)
arcNarrow rel base = fmap (\(_, arc, _) -> arc) (arcRelative rel base)

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

-- | Calculate the intersection of two extents.
extentIntersect :: Extent -> Extent -> Extent
extentIntersect (Extent a1) (Extent a2) = Extent (fromMaybe (Arc 0 0) (arcIntersect a1 a2))

-- | Get the positive portion of an extent as an arc.
-- If non-empty in positive time, returns an arc starting at 0.
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
