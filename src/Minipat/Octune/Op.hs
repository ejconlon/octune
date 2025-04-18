module Minipat.Octune.Op where

import Bowtie (Anno (..), Fix, Memo (..), cataM, memoCataM, memoFix, memoKey, memoVal, mkMemo, mkMemoM)
import Control.Monad (when)
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Identity (Identity (..), runIdentity)
import Control.Monad.Primitive (PrimMonad (..), RealWorld)
import Control.Monad.Reader (asks)
import Control.Monad.Trans (lift)
import Dahdit (ElemCount (..))
import Data.Foldable (fold, foldl', for_, toList)
import Data.Functor.Foldable (cata)
import Data.Int (Int32)
import Data.Map.Strict (Map)
import Data.Primitive (Prim)
import Data.Primitive.PrimArray
  ( MutablePrimArray
  , PrimArray
  , copyMutablePrimArray
  , copyPrimArray
  , getSizeofMutablePrimArray
  , newPrimArray
  , setPrimArray
  , sizeofPrimArray
  , unsafeFreezePrimArray
  )
import Data.Primitive.PrimVar (PrimVar, newPrimVar, readPrimVar, writePrimVar)
import Data.Semigroup (Sum (..))
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Minipat.Octune.InternalSamples
  ( InternalSamples (..)
  , isampsConcat
  , isampsConcatReps
  , isampsConstant
  , isampsEmpty
  , isampsLength
  , isampsMix
  , isampsTrim
  )
import Minipat.Octune.PrimPar (Mutex, ParMonad, PrimPar, newMutex, runPrimPar, withMutex)
import Minipat.Octune.Time
  ( Arc (..)
  , Delta
  , Extent (..)
  , Measure (..)
  , Overlap (..)
  , QDelta (..)
  , QTime (..)
  , Rate
  , Reps
  , Time
  , arcDeltaCoverMax
  , arcFrom
  , arcIntersect
  , arcLen
  , arcNarrow
  , arcNull
  , arcOverlap
  , arcRelative
  , arcShift
  , arcSkip
  , extentConcat
  , extentEmpty
  , extentFromDelta
  , extentLen
  , extentMerge
  , extentPosArc
  , extentRepeat
  , extentShift
  , quantizeArc
  , quantizeDelta
  , unquantizeArc
  , unquantizeDelta
  )
import Minipat.Octune.Topo (SortErr, topoAnnoM, topoEval)
import Minipat.Octune.Util (memoRecallM, mergeMutableIntoPrimArray, zeroPrimArray)

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

-- | A fixed point of the OpF functor, annotated with its extent.
type OpAnno n = Memo (OpF n) Extent

opRefs :: (Ord n) => Op n -> Set n
opRefs = cata opRefsF

opAnnoRefs :: (Ord n) => Memo (OpF n) k -> Set n
opAnnoRefs = opRefs . memoFix

opRefsF :: (Ord n) => OpF n (Set n) -> Set n
opRefsF = \case
  OpRef n -> Set.singleton n
  op -> fold op

-- | Infer the extent of an operation.
opInferExtentF :: (Monad m) => Rate -> (n -> m Extent) -> OpF n Extent -> m Extent
opInferExtentF rate onRef = \case
  OpEmpty -> pure extentEmpty
  OpSamp x ->
    let i = isampsLength x
    in  pure $
          if i == 0
            then extentEmpty
            else extentFromDelta (unquantizeDelta rate (fromIntegral i))
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
opInferExtentTopo rate m = topoEval opRefs m (\onK _ -> opInferExtent rate onK)

-- | Annotate an operation with its extent.
opAnnoExtent :: (Monad m) => Rate -> (n -> m Extent) -> Op n -> m (OpAnno n)
opAnnoExtent rate f = mkMemoM (opInferExtentF rate f)

-- | Annotate an operation with its extent, handling references with Left.
opAnnoExtentSingle :: Rate -> Op n -> Either n (OpAnno n)
opAnnoExtentSingle rate = opAnnoExtent rate Left

-- | Annotate multiple operations with their extents in topological order.
opAnnoExtentTopo :: (Ord n) => Rate -> Map n (Op n) -> Either (SortErr n) (Map n (OpAnno n))
opAnnoExtentTopo rate m = fmap runIdentity (topoAnnoM opRefsF (fmap (mkMemo (const ())) m) (\onK _ _ fx -> opInferExtentF rate onK fx))

-- | A type representing audio samples.
newtype Samples = Samples {runSamples :: Arc QTime -> InternalSamples}

-- | Create empty samples for a given arc.
arrEmptyArc :: Rate -> Arc QTime -> InternalSamples
arrEmptyArc rate = arrEmptyDelta rate . arcLen

-- | Create empty samples for a given delta.
arrEmptyDelta :: Rate -> QDelta -> InternalSamples
arrEmptyDelta _ d = isampsConstant (fromIntegral d) 0

-- | Create an empty sample generator.
orEmpty :: Rate -> Samples
orEmpty = Samples . arrEmptyArc

-- | Create a sample generator from constant samples.
orSamp :: Rate -> InternalSamples -> Samples
orSamp rate x =
  let i = fromIntegral (isampsLength x)
  in  if i == 0
        then orEmpty rate
        else Samples $ \(Arc s e) ->
          let s' = max 0 s
              e' = min i e
              l = e - s
              l' = e' - s'
          in  if s' >= i || e' <= 0 || s' >= e'
                then isampsConstant (fromIntegral l) 0
                else
                  let preSamps = isampsConstant (fromIntegral (s' - s)) 0
                      postSamps = isampsConstant (fromIntegral (e - e')) 0
                      bodySamps = isampsTrim (fromIntegral s') (fromIntegral l') x
                      samps = [preSamps, bodySamps, postSamps]
                  in  isampsConcat samps

-- | Create a sample generator that repeats another generator.
orRepeat :: Rate -> Reps -> Extent -> Samples -> Samples
orRepeat rate n rext rsamp =
  if n <= 0
    then Samples (arrEmptyArc rate)
    else case extentPosArc rext of
      Nothing -> Samples (arrEmptyArc rate)
      Just sarc ->
        let repLenQ = quantizeDelta rate (arcLen sarc)
            repLenU = unquantizeDelta rate repLenQ
        in  Samples $ \arc@(Arc _ _) ->
              let arc' = unquantizeArc rate arc
                  targetLenQ :: QDelta = arcLen arc -- Explicit type annotation
              in  if targetLenQ <= 0 || repLenU <= 0
                    then arrEmptyArc rate arc
                    else case arcDeltaCoverMax repLenU n arc' of
                      Nothing -> arrEmptyArc rate arc -- No overlap
                      Just (paddingBeforeU, firstN, cappedN, _) ->
                        -- Ignore paddingAfterT
                        if firstN >= cappedN
                          then arrEmptyArc rate arc -- No repetitions overlap
                          else do
                            -- Check if there are any repetitions to process
                            let prePaddingQ = quantizeDelta rate paddingBeforeU

                            -- Analyze first and last repetitions
                            let firstRepIdx = firstN
                                lastRepIdx = cappedN - 1
                                firstRepStartDeltaU = fromIntegral firstRepIdx * repLenU
                                firstRepStartU = addDelta 0 firstRepStartDeltaU
                                firstRepArcU = arcFrom firstRepStartU repLenU
                                mFirstIntersectU = arcIntersect arc' firstRepArcU

                                lastRepStartDeltaU = fromIntegral lastRepIdx * repLenU
                                lastRepStartU = addDelta 0 lastRepStartDeltaU
                                lastRepArcU = arcFrom lastRepStartU repLenU
                                mLastIntersectU = arcIntersect arc' lastRepArcU

                            -- Determine relative arcs for partial segments
                            let calcRelArc intersectDelta startDelta =
                                  case intersectDelta of
                                    Nothing -> Nothing
                                    Just subArcU -> Just (arcShift subArcU (negate startDelta))

                            let mFirstRelArcU = calcRelArc mFirstIntersectU firstRepStartDeltaU
                                mLastRelArcU = calcRelArc mLastIntersectU lastRepStartDeltaU

                            -- Quantize relative arcs to check lengths deterministically
                            let mFirstRelArcQ = quantizeArc rate <$> mFirstRelArcU
                                mLastRelArcQ = quantizeArc rate <$> mLastRelArcU

                            -- Check if first/last are partial based on quantized length
                            let isFirstPartial = maybe False (\qArc -> arcLen qArc < repLenQ) mFirstRelArcQ
                                isLastPartial = maybe False (\qArc -> arcLen qArc < repLenQ) mLastRelArcQ

                            -- Calculate number of full repetitions
                            let numTotalReps = cappedN - firstN
                                numPartials = (if isFirstPartial then 1 else 0) + (if firstRepIdx /= lastRepIdx && isLastPartial then 1 else 0)
                                numFullReps = max 0 (numTotalReps - numPartials)
                            -- firstFullRepIdx = firstRepIdx + (if isFirstPartial then 1 else 0) -- Not needed directly

                            -- Render required segments (max 3)
                            let mPrefixSamps = if isFirstPartial then renderSegment rate rsamp =<< mFirstRelArcU else Nothing
                                mFullSamps = if numFullReps > 0 then renderSegment rate rsamp (arcFrom 0 repLenU) else Nothing -- Use arcFrom
                                mSuffixSamps = if firstRepIdx /= lastRepIdx && isLastPartial then renderSegment rate rsamp =<< mLastRelArcU else Nothing

                            -- Assemble using isampsConcatReps
                            let paddingSeg = [(1 :: Int, arrEmptyDelta rate prePaddingQ) | prePaddingQ > 0]
                                prefixSeg = [(1 :: Int, samps) | Just samps <- [mPrefixSamps]]
                                fullSeg =
                                  [ (fromIntegral numFullReps, samps)
                                  | numFullReps > 0
                                  , Just samps <- [mFullSamps]
                                  ]
                                suffixSeg = [(1 :: Int, samps) | Just samps <- [mSuffixSamps]]
                                segmentsToConcat = paddingSeg ++ prefixSeg ++ fullSeg ++ suffixSeg

                            -- Calculate total length and add padding if needed
                            let currentLen :: QDelta = fromIntegral $ getSum $ foldMap (Sum . (\(c, s) -> c * unElemCount (isampsLength s))) segmentsToConcat -- Calculate as Int/ElemCount sum, then convert
                                paddingNeeded = targetLenQ - currentLen
                                endPaddingSeg = [(1 :: Int, arrEmptyDelta rate paddingNeeded) | paddingNeeded > 0]

                            isampsConcatReps (segmentsToConcat ++ endPaddingSeg)

-- Helper to calculate and render a segment
renderSegment :: Rate -> Samples -> Arc Time -> Maybe InternalSamples
renderSegment rate sampler relArcU =
  let relArcQ = quantizeArc rate relArcU
  in  if arcNull relArcQ then Nothing else Just (runSamples sampler relArcQ)

-- | Create a sample generator that slices another generator.
orSlice :: Rate -> Arc Time -> Samples -> Samples
orSlice rate sarc rsamp =
  if arcNull sarc
    then Samples (arrEmptyArc rate)
    else Samples $ \arc ->
      -- NOTE: We must use arcRelative here because the current arc and the slice arc
      -- are essentially in different frames of reference.
      case arcRelative (unquantizeArc rate arc) sarc of
        Just (before, arc', after) ->
          let preSamps = arrEmptyDelta rate (quantizeDelta rate before)
              bodySamps = runSamples rsamp (quantizeArc rate arc')
              postSamps = arrEmptyDelta rate (quantizeDelta rate after)
              samps = [preSamps, bodySamps, postSamps]
          in  isampsConcat samps
        Nothing -> arrEmptyArc rate arc

-- | Create a sample generator that shifts another generator in time.
orShift :: Rate -> Delta -> Samples -> Samples
orShift rate c rsamp = Samples $ \arc ->
  runSamples rsamp (quantizeArc rate (arcShift (unquantizeArc rate arc) c))

-- | Create a sample generator that concatenates multiple generators.
orConcat :: Rate -> Seq (Anno Extent Samples) -> Samples
orConcat rate rs =
  let gather dta@(d, t, a) (Anno l g) =
        let q = quantizeDelta rate (extentLen l)
        in  if q <= 0
              then dta
              else
                let d' = d + q
                    t' = addDelta t q
                in  (d', t', a :|> (d, Arc t t', g))
      (_, tot, subArcs) = foldl' gather (0, 0, Empty) rs
      whole = Arc 0 tot
  in  Samples $ \arc ->
        let elemLen = arcLen arc
        in  case arcOverlap arc whole of
              OverlapOn before filtArc after ->
                let preSamps = arrEmptyDelta rate before
                    postSamps = arrEmptyDelta rate after
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
              _ -> isampsConstant (fromIntegral elemLen) 0

-- | Create a sample generator that merges multiple generators by mixing their samples.
orMerge :: [Samples] -> Samples
orMerge rsamps = Samples (\arc -> isampsMix (fmap (`runSamples` arc) rsamps))

-- | Render an operation to samples.
opRender :: (Monad m) => Rate -> (n -> m Samples) -> OpAnno n -> m Samples
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
opRenderTopo :: (Ord n) => Rate -> Map n (OpAnno n) -> Either (SortErr n) (Map n Samples)
opRenderTopo rate m = fmap (fmap runIdentity) (topoEval opAnnoRefs m (\onK _ -> opRender rate onK))

-- | Render an operation to samples, handling references with Left.
opRenderSingleOn :: Rate -> Op n -> Arc Time -> Either n InternalSamples
opRenderSingleOn rate op arc = do
  op' <- opAnnoExtentSingle rate op
  samps <- opRender rate Left op'
  pure (runSamples samps (quantizeArc rate arc))

-- | Render an operation to samples, handling references with Left.
opRenderSingle :: Rate -> Op n -> Either n InternalSamples
opRenderSingle rate op = do
  op' <- opAnnoExtentSingle rate op
  samps <- opRender rate Left op'
  pure (maybe isampsEmpty (runSamples samps . quantizeArc rate) (extentPosArc (memoKey op')))

-- | Simplify an operation through empty propagation and shift/repeat identities.
opSimplifyF :: (Monad m) => (n -> m (OpAnno n)) -> OpAnno n -> m (OpAnno n)
opSimplifyF onRef = memoCataM go
 where
  isOpEmpty = \case
    OpEmpty -> True
    _ -> False
  extented op = asks (`MemoP` op)
  emptied = pure (MemoP extentEmpty OpEmpty)
  go op = case op of
    OpEmpty -> emptied
    OpSamp _ -> extented op
    OpShift delta inner ->
      case memoVal inner of
        OpEmpty -> emptied
        _ ->
          if delta == 0
            then pure inner
            else extented op
    OpRepeat n inner ->
      if
        | n <= 0 -> emptied
        | n == 1 -> pure inner
        | otherwise ->
            case memoVal inner of
              OpEmpty -> emptied
              _ -> extented op
    OpSlice arc inner ->
      extented $ case arcIntersect (unExtent (memoKey inner)) arc of
        Nothing -> OpSlice arc (MemoP extentEmpty OpEmpty)
        Just _ -> op
    OpConcat ops ->
      case Seq.filter (not . isOpEmpty . memoVal) ops of
        Empty -> emptied
        inner :<| Empty -> pure inner
        ops' -> extented (OpConcat ops')
    OpMerge ops ->
      case Seq.filter (not . isOpEmpty . memoVal) ops of
        Empty -> emptied
        inner :<| Empty -> pure inner
        ops' -> extented (OpMerge ops')
    OpRef n -> do
      inner <- lift (onRef n)
      if isOpEmpty (memoVal inner)
        then emptied
        else extented op

opSimplifyTopo :: (Ord n) => Map n (OpAnno n) -> Either (SortErr n) (Map n (Either n (OpAnno n)))
opSimplifyTopo m = topoEval opAnnoRefs m (\onK _ -> opSimplifyF onK)

opSimplifySingle :: OpAnno n -> Either n (OpAnno n)
opSimplifySingle = opSimplifyF Left

-- | A view of an array with bounds.
-- From the outside, index 0 corresponds to the start index of the bounds.
-- The length of the view is the same as the length of the bounds.
-- Invariants:
-- * The actual array length is GTE the bounds length.
-- * Start time is a valid array index.
data View z = View
  { viewBounds :: !(Arc QTime)
  -- ^ The bounds of the view
  , viewArray :: !z
  -- ^ The array being viewed
  }
  deriving stock (Eq, Ord, Show)

-- | Get the length of a view.
viewLen :: View z -> QDelta
viewLen = arcLen . viewBounds

-- | Check if a view is empty.
viewNull :: View z -> Bool
viewNull = arcNull . viewBounds

-- | Skip a number of elements from the start of a view.
viewSkip :: QDelta -> View z -> Maybe (View z)
viewSkip skip (View bounds arr) = fmap (`View` arr) (arcSkip skip bounds)

-- | Narrow an immutable view to a relative sub-range.
viewNarrow :: Arc QTime -> View z -> Maybe (View z)
viewNarrow rel (View bounds arr) = fmap (`View` arr) (arcNarrow rel bounds)

-- | Narrow a mutable view to a relative sub-range.
mavNarrow :: Arc QTime -> MutArrayView a -> Maybe (MutArrayView a)
mavNarrow rel (View bounds mut) = fmap (`View` mut) (arcNarrow rel bounds)

-- | A view of an immutable primitive array.
type ArrayView a = View (PrimArray a)

-- | Create a view of a primitive array.
avNew :: (Prim a) => PrimArray a -> ArrayView a
avNew arr = View (Arc 0 (fromIntegral (sizeofPrimArray arr))) arr

-- | A view of a mutable primitive array.
type MutArrayView a = View (Mutex (MutablePrimArray RealWorld a))

-- | Create a view of a mutable primitive array.
mavNew :: (ParMonad m, Prim a) => MutablePrimArray RealWorld a -> m (MutArrayView a)
mavNew arr = do
  len <- getSizeofMutablePrimArray arr
  mut <- newMutex arr
  pure (View (arcFrom 0 (fromIntegral len)) mut)

-- | Copy from an array view to a mutable array view.
mavCopy :: (ParMonad m, Prim a) => MutArrayView a -> ArrayView a -> m ()
mavCopy (View darc@(Arc dstart _) darr) (View sarc@(Arc sstart _) sarr) = do
  let slen = arcLen sarc
      dlen = arcLen darc
      len = min slen dlen
  withMutex darr $ \marr -> do
    mlen <- getSizeofMutablePrimArray marr
    let bad =
          len <= 0
            || dstart < 0
            || sstart < 0
            || fromIntegral dlen > mlen
            || fromIntegral dstart + len > dlen
            || fromIntegral sstart + len > slen
    if bad
      then error "mavCopy: out of bounds"
      else copyPrimArray marr (fromIntegral dstart) sarr (fromIntegral sstart) (fromIntegral len)

-- | Copy a segment within a mutable array view.
copyWithinMav :: (ParMonad m, Prim a) => MutArrayView a -> QTime -> QTime -> QDelta -> m ()
copyWithinMav (View darc@(Arc mavS _) mavMutex) srcRelOffset destRelOffset copyLen = do
  let dlen = arcLen darc
  let srcAbsOffset = mavS + srcRelOffset
      destAbsOffset = mavS + destRelOffset
  withMutex mavMutex $ \marr -> do
    mlen <- getSizeofMutablePrimArray marr
    let bad =
          copyLen <= 0
            || srcAbsOffset < 0
            || destAbsOffset < 0
            || fromIntegral dlen > mlen
            || fromIntegral srcAbsOffset + fromIntegral copyLen > mlen
            || fromIntegral destAbsOffset + fromIntegral copyLen > mlen
    if bad
      then error "copyWithinMav: out of bounds"
      else copyMutablePrimArray marr (fromIntegral destAbsOffset) marr (fromIntegral srcAbsOffset) (fromIntegral copyLen)

-- | A type representing mutable samples.
newtype MutSamples = MutSamples {runMutSamples :: Arc QTime -> MutArrayView Int32 -> PrimPar ()}

-- | Run mutable samples to produce a primitive array.
runMutSamplesSimple :: Rate -> MutSamples -> Arc QTime -> IO InternalSamples
runMutSamplesSimple _rate samps arc = do
  let elemsLen = arcLen arc
  arr <- zeroPrimArray (fromIntegral elemsLen)
  runPrimPar $ do
    mav <- mavNew arr
    runMutSamples samps arc mav
  fmap InternalSamples (unsafeFreezePrimArray arr)

-- | Create empty mutable samples.
ormEmpty :: MutSamples
ormEmpty = MutSamples (\_ _ -> pure ())

-- | Create mutable samples from constant samples.
ormSamp :: InternalSamples -> MutSamples
ormSamp isamps =
  let srcArr = unInternalSamples isamps
      srcLen = sizeofPrimArray srcArr -- Length in elements (QDelta)
  in  if srcLen == 0
        then ormEmpty
        else MutSamples $ \(Arc s e) (View (Arc mavS _) mavMutex) -> do
          let i = fromIntegral srcLen -- Source length as QTime

              -- Determine the valid range of the requested arc clamped to the source bounds [0, i)
              s' = max 0 s -- Clamp start to 0
              e' = min i e -- Clamp end to i
              copyLen = e' - s' -- Length of the valid segment to copy

          -- Only proceed if there's overlap
          when (copyLen > 0) $ do
            -- Offset within the source array to start reading from
            let srcOffset = s' -- Start reading from the clamped start index

            -- Offset within the *requested arc* where the valid data starts.
            -- This tells us where in the target view (mav) to start writing.
            let arcValidDataOffset = s' - s -- Always >= 0

            -- Absolute offset within the *target mav's underlying array* to start writing to
            let mavTargetOffset = mavS + arcValidDataOffset

            -- Perform the copy using calculated offsets and length
            withMutex mavMutex $ \marr ->
              copyPrimArray marr (fromIntegral mavTargetOffset) srcArr (fromIntegral srcOffset) (fromIntegral copyLen)

-- | Create mutable samples that slice another generator.
ormSlice :: Rate -> Arc Time -> MutSamples -> MutSamples
ormSlice rate sarc rsamp = MutSamples $ \arc mav -> do
  -- Calculate the relative overlap between the requested arc and the slice arc
  let relResult = arcRelative (unquantizeArc rate arc) sarc
  case relResult of
    Nothing -> pure () -- No overlap, do nothing to mav
    Just (beforeU, arcU, _afterU) -> do
      -- Ignore afterU
      let arcQ = quantizeArc rate arcU -- Arc to request from inner sampler
          beforeQ = quantizeDelta rate beforeU -- Offset into the requested `arc` where sliced data starts
      let writeLen = arcLen arcQ
      -- Check if the part we need to render has any length
      when (writeLen > 0) $ do
        -- Calculate the relative arc within mav where we need to write.
        -- `mav` represents the buffer for the requested `arc`.
        -- The data starts `beforeQ` into this buffer.
        let relativeWriteArc = arcFrom (QTime (unQDelta beforeQ)) writeLen

        -- Narrow the target view to this specific relative arc
        let mavNarrowed = mavNarrow relativeWriteArc mav

        for_ mavNarrowed $ \mavN -> do
          -- Run the inner sampler for its corresponding quantized arc, writing into the narrowed view
          runMutSamples rsamp arcQ mavN

-- | Create mutable samples that shift another generator in time.
ormShift :: Rate -> Delta -> MutSamples -> MutSamples
ormShift rate c rsamp = MutSamples $ \arc mav ->
  let shiftedArcU = arcShift (unquantizeArc rate arc) c
      shiftedArcQ = quantizeArc rate shiftedArcU
  in  runMutSamples rsamp shiftedArcQ mav

-- | Create mutable samples that concatenate multiple generators.
ormConcat :: Rate -> Seq (Anno Extent MutSamples) -> MutSamples
ormConcat rate rsampsAn =
  -- Precompute offsets and arcs for sub-samplers, similar to orConcat
  let gather dta@(d, t, a) anno@(Anno l _g) =
        let q = quantizeDelta rate (extentLen l)
        in  if q <= 0
              then dta
              else
                let d' = d + q
                    tU = extentLen l -- Unquantized length for time calculation
                    t' = addDelta t tU
                in  (d', t', a :|> (d, anno)) -- Store QDelta start and the whole Anno
      (_, _, subItems) = foldl' gather (0, 0 :: Time, Empty) rsampsAn
      -- Calculate the total quantized length
      totalLenQ = getSum $ foldMap (Sum . quantizeDelta rate . extentLen . annoKey) rsampsAn
      wholeQ = arcFrom (QTime 0) totalLenQ -- Arc QTime
  in  MutSamples $ \targetArcQ mav ->
        -- Check overlap between the requested target arc and the total extent
        case arcOverlap targetArcQ wholeQ of
          OverlapLt -> pure () -- Request is entirely before the concatenation
          OverlapGt -> pure () -- Request is entirely after the concatenation
          OverlapOn _ overlapArcQ _ ->
            -- Iterate through the sub-samplers
            for_ subItems $ \(subStartQ, subAnno) -> do
              let subExtent = annoKey subAnno
                  subSampler = annoVal subAnno
                  subLenQ = quantizeDelta rate (extentLen subExtent)
                  subArcQ = arcFrom (QTime (unQDelta subStartQ)) subLenQ -- Arc QTime

              -- Find the intersection between the overall overlap arc and this sub-sampler's arc
              case arcIntersect overlapArcQ subArcQ of
                Nothing -> pure () -- This sub-sampler doesn't overlap the required section
                Just intersectArcQ ->
                  -- Calculate the arc relative to the sub-sampler's start time
                  let subRelativeArcQ = arcShift intersectArcQ (negate subStartQ)
                      -- Calculate the arc relative to the target mav's start time
                      targetRelativeOffset = arcStart intersectArcQ - arcStart targetArcQ
                      targetRelativeArcQ = arcFrom targetRelativeOffset (arcLen intersectArcQ)
                  in  -- Narrow the target view and run the sub-sampler
                      for_ (mavNarrow targetRelativeArcQ mav) $ \mavN ->
                        runMutSamples subSampler subRelativeArcQ mavN

-- | Create mutable samples that merge multiple generators by mixing their samples.
ormMerge :: [MutSamples] -> MutSamples
ormMerge childSamps =
  if null childSamps
    then ormEmpty
    else MutSamples $ \requestedArcQ mav -> do
      let mavBounds = viewBounds mav
          mavLen = arcLen mavBounds
          mavStart = arcStart mavBounds

      when (mavLen > 0) $ do
        -- Create a temporary buffer for the child's output
        tempArr <- newPrimArray (fromIntegral mavLen)
        tempMav <- mavNew tempArr -- Has bounds Arc 0 mavLen

        -- Iterate through child samplers
        for_ childSamps $ \childSamp -> do
          -- Clear the temporary buffer
          withMutex (viewArray tempMav) $ \marr ->
            setPrimArray marr 0 (fromIntegral mavLen) 0

          -- Run the child sampler into the temporary buffer
          runMutSamples childSamp requestedArcQ tempMav

          -- Merge the temporary buffer into the main target view
          withMutex (viewArray mav) $ \targetArr ->
            mergeMutableIntoPrimArray (+) targetArr (fromIntegral mavStart) tempArr 0 (fromIntegral mavLen)

-- | Create mutable samples that repeat another generator.
ormRepeat :: Rate -> Reps -> Extent -> MutSamples -> MutSamples
ormRepeat rate reps childExtent childSamp =
  if reps <= 0
    then ormEmpty
    else case extentPosArc childExtent of
      Nothing -> ormEmpty -- Child has no positive extent
      Just childArcU ->
        let repLenU = arcLen childArcU
            repLenQ = quantizeDelta rate repLenU
        in  if repLenU <= 0 || repLenQ <= 0
              then ormEmpty -- Repetition length is zero or negative
              else MutSamples $ \targetArcQ mav -> do
                let targetArcU = unquantizeArc rate targetArcQ
                let coverResult = arcDeltaCoverMax repLenU reps targetArcU

                case coverResult of
                  Nothing -> pure () -- No overlap
                  Just (paddingBeforeU, firstN, cappedN, _paddingAfterU) -> do
                    -- Quantized padding at the start *within* the targetArcQ
                    let prePaddingQ = quantizeDelta rate paddingBeforeU
                        numTotalReps = cappedN - firstN

                    when (numTotalReps > 0) $ do
                      -- Calculate indices and repetition arcs
                      let firstRepIdx = firstN
                          lastRepIdx = cappedN - 1
                          fullRepArcU = arcFrom 0 repLenU -- Relative arc for a full repetition
                          fullRepArcQ = quantizeArc rate fullRepArcU

                      -- Determine if first/last reps are partial
                      let firstRepStartDeltaU = fromIntegral firstRepIdx * repLenU
                          firstRepStartU = addDelta 0 firstRepStartDeltaU
                          firstRepArcU = arcFrom firstRepStartU repLenU
                          mFirstIntersectU = arcIntersect targetArcU firstRepArcU
                          mFirstRelArcU = arcShift <$> mFirstIntersectU <*> pure (negate firstRepStartDeltaU)
                          mFirstRelArcQ = quantizeArc rate <$> mFirstRelArcU
                          isFirstPartial = maybe False (\qArc -> arcLen qArc < repLenQ) mFirstRelArcQ

                      let lastRepStartDeltaU = fromIntegral lastRepIdx * repLenU
                          lastRepStartU = addDelta 0 lastRepStartDeltaU
                          lastRepArcU = arcFrom lastRepStartU repLenU
                          mLastIntersectU = arcIntersect targetArcU lastRepArcU
                          mLastRelArcU = arcShift <$> mLastIntersectU <*> pure (negate lastRepStartDeltaU)
                          mLastRelArcQ = quantizeArc rate <$> mLastRelArcU
                          isLastPartial = firstRepIdx /= lastRepIdx && maybe False (\qArc -> arcLen qArc < repLenQ) mLastRelArcQ

                      -- Calculate number of full repetitions
                      let numPartials = (if isFirstPartial then 1 else 0) + (if isLastPartial then 1 else 0)
                          numFullReps = max 0 (numTotalReps - numPartials)

                      -- Iterate and render directly into mav
                      -- Note: mav's bounds are relative to the *requested* targetArcQ.
                      -- Offsets calculated here need to be relative to mav's bounds start.
                      let mavStartOffset = arcStart (viewBounds mav)
                          initialWritePos = addDelta mavStartOffset prePaddingQ

                      currentWritePosRef :: PrimVar (PrimState PrimPar) Int <- newPrimVar (fromInteger (unQTime initialWritePos))

                      -- Helper to read QTime from PrimVar Int
                      let readWritePos :: PrimPar QTime
                          readWritePos = QTime . fromIntegral <$> readPrimVar currentWritePosRef
                      -- Helper to write QTime to PrimVar Int
                      let writeWritePos :: QTime -> PrimPar ()
                          writeWritePos qtime = writePrimVar currentWritePosRef (fromInteger (unQTime qtime))

                      -- 1. Render first partial repetition
                      when isFirstPartial $ for_ mFirstRelArcQ $ \firstRelQ -> do
                        let len = arcLen firstRelQ
                        when (len > 0) $ do
                          currentPos <- readWritePos
                          -- The target arc must be relative to mav's bounds start
                          let relativeTargetArc = arcFrom (currentPos - mavStartOffset) len
                          for_ (mavNarrow relativeTargetArc mav) $ \mavN ->
                            runMutSamples childSamp firstRelQ mavN
                          writeWritePos (addDelta currentPos len)

                      -- 2. Render full repetitions
                      when (numFullReps > 0 && arcLen fullRepArcQ > 0) $ do
                        let fullLen = arcLen fullRepArcQ
                        firstFullRepPos <- readWritePos
                        -- The target arc must be relative to mav's bounds start
                        let firstRelativeTargetArc = arcFrom (firstFullRepPos - mavStartOffset) fullLen
                        for_ (mavNarrow firstRelativeTargetArc mav) $ \mavN ->
                          runMutSamples childSamp fullRepArcQ mavN
                        let posAfterFirst = addDelta firstFullRepPos fullLen
                        writeWritePos posAfterFirst

                        -- Copy for the remaining full repetitions
                        when (numFullReps > 1) $ do
                          -- copyWithinMav expects offsets relative to the start of the MutArrayView
                          let firstFullRepRelOffset = firstFullRepPos - mavStartOffset
                          for_ [1 .. numFullReps - 1] $ \_ -> do
                            currentPos <- readWritePos
                            let currentRelOffset = currentPos - mavStartOffset
                            copyWithinMav mav firstFullRepRelOffset currentRelOffset fullLen
                            let nextPosAfterCopy = addDelta currentPos fullLen
                            writeWritePos nextPosAfterCopy

                      -- 3. Render last partial repetition
                      when isLastPartial $ for_ mLastRelArcQ $ \lastRelQ -> do
                        let len = arcLen lastRelQ
                        when (len > 0) $ do
                          currentPos <- readWritePos
                          -- The target arc must be relative to mav's bounds start
                          let relativeTargetArc = arcFrom (currentPos - mavStartOffset) len
                          for_ (mavNarrow relativeTargetArc mav) $ \mavN ->
                            runMutSamples childSamp lastRelQ mavN
                          writeWritePos (addDelta currentPos len)

-- | Render an operation to mutable samples.
opRenderMut :: (Monad m) => Rate -> (n -> m MutSamples) -> OpAnno n -> m MutSamples
opRenderMut rate onRef = memoRecallM go
 where
  go = \case
    OpEmpty -> pure ormEmpty
    OpSamp x -> pure (ormSamp x)
    OpRepeat n r -> pure (ormRepeat rate n (annoKey r) (annoVal r))
    OpSlice a r -> pure (ormSlice rate a (annoVal r))
    OpShift c r -> pure (ormShift rate c (annoVal r))
    OpConcat rs -> pure (ormConcat rate rs)
    OpMerge rs -> pure (ormMerge (fmap annoVal (toList rs)))
    OpRef n -> lift (onRef n)

-- | Render multiple operations to mutable samples in topological order.
opRenderMutTopo :: (Ord n) => Rate -> Map n (OpAnno n) -> Either (SortErr n) (Map n MutSamples)
opRenderMutTopo rate m = topoEval opAnnoRefs m (\onK _ -> runIdentity . opRenderMut rate (Identity . onK))

-- | Render an operation to mutable samples, handling references with Left.
opRenderMutSingle :: Rate -> Op n -> IO (Either n InternalSamples)
opRenderMutSingle rate op = runExceptT $ do
  op' <- either throwError pure (opAnnoExtentSingle rate op)
  samps <- opRenderMut rate throwError op'
  case extentPosArc (memoKey op') of
    Nothing -> pure isampsEmpty
    Just arc -> do
      let arc' = quantizeArc rate arc
      liftIO (runMutSamplesSimple rate samps arc')

opRenderMutSingleOn :: Rate -> Op n -> Arc Time -> IO (Either n InternalSamples)
opRenderMutSingleOn rate op arc = runExceptT $ do
  op' <- either throwError pure (opAnnoExtentSingle rate op)
  samps <- opRenderMut rate throwError op'
  let arc' = quantizeArc rate arc
  liftIO (runMutSamplesSimple rate samps arc')
