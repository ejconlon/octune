module Data.Sounds where

import Bowtie (Anno (..), Fix (..), Memo, cataM, memoKey, mkMemoM, pattern MemoP)
import Control.DeepSeq (NFData)
import Control.Monad (unless, when, (>=>))
import Control.Monad.Primitive (PrimMonad (..))
import Control.Monad.Reader (ReaderT (..))
import Control.Monad.Trans (lift)
import Dahdit.Audio.Binary (QuietLiftedArray (..))
import Dahdit.Audio.Wav.Simple (WAVE (..), WAVEHeader (..), WAVESamples (..), getWAVEFile, putWAVEFile)
import Dahdit.LiftedPrimArray (LiftedPrimArray (..))
import Dahdit.Sizes (ByteCount (..), ElemCount (..))
import Data.Foldable (fold, foldl', for_, toList)
import Data.Functor.Foldable (Recursive (..), cata)
import Data.Int (Int32)
import Data.Map.Strict (Map)
import Data.Primitive.ByteArray (ByteArray (..))
import Data.Primitive.PrimArray
  ( MutablePrimArray
  , PrimArray (..)
  , clonePrimArray
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
  , writePrimArray
  )
import Data.Primitive.Types (Prim)
import Data.STRef.Strict (newSTRef, readSTRef, writeSTRef)
import Data.Semigroup (Max (..), Sum (..))
import Data.Sequence (Seq (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Topo (SortErr (..), topoAnnoM, topoEval)
import Paths_octune (getDataFileName)
import System.Directory (doesFileExist)
import System.IO.Unsafe (unsafePerformIO)

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

-- TODO support Rational reps
newtype Reps = Reps {unReps :: Integer}
  deriving stock (Eq, Show)
  deriving newtype (Num, Ord, Enum, Real, Integral)
  deriving (Semigroup, Monoid) via (Sum Integer)

mulReps :: (Num d) => Reps -> d -> d
mulReps (Reps n) d = fromInteger n * d

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
  in  if s2 <= e1 || s1 <= e2 then Just (Arc s3 e3) else Nothing

arcLen :: (Measure t d) => Arc t -> d
arcLen (Arc s e) = measureDelta s e

arcFrom :: (Measure t d) => t -> d -> Arc t
arcFrom t d = Arc t (addDelta t d)

arcNull :: (Measure t d) => Arc t -> Bool
arcNull = (0 ==) . arcLen

arcShift :: (Measure t d) => Arc t -> d -> Arc t
arcShift (Arc s e) d =
  let d' = negate d
  in  Arc (addDelta s d') (addDelta e d')

arcReps :: (Measure t d) => Arc t -> Reps -> Arc t
arcReps (Arc s e) n = Arc s (addDelta s (mulReps n (measureDelta s e)))

data Overlap t = OverlapLt | OverlapEq !(Arc t) | OverlapGt
  deriving stock (Eq, Ord, Show)

arcOverlap :: (Ord t) => Arc t -> Arc t -> Overlap t
arcOverlap (Arc ns ne) (Arc hs he) =
  if
    | ne <= hs -> OverlapLt
    | ns >= he -> OverlapGt
    | otherwise -> OverlapEq (Arc (max ns hs) (min ne he))

data OpF n r
  = OpEmpty
  | OpSamp !InternalSamples
  | OpShift !Delta r
  | OpSlice !Reps !(Arc Time) r
  | OpConcat !(Seq r)
  | OpMerge !(Seq r)
  | OpRef !n
  deriving stock (Eq, Show, Functor, Foldable, Traversable)

type Op n = Fix (OpF n)

opRefs :: (Ord n) => Op n -> Set n
opRefs = cata $ \case
  OpRef n -> Set.singleton n
  op -> fold op

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
  if s >= e || e <= 0
    then Nothing
    else Just (Arc 0 e)

extentLen :: Extent -> Delta
extentLen = maybe 0 arcLen . extentPosArc

extentShift :: Extent -> Delta -> Extent
extentShift (Extent arc) c = Extent (if arcNull arc then arc else arcShift arc c)

extentSlice :: Reps -> Arc Time -> Extent
extentSlice n arc = mkExtent (arcFrom 0 (arcLen (arcReps arc n)))

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
  OpSlice n sarc _ -> pure (extentSlice n sarc)
  OpShift c r -> pure (extentShift r c)
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

opRender :: (Monad m) => Rate -> (n -> m Samples) -> Memo (OpF n) Extent -> m Samples
opRender rate onRef = goTop
 where
  goTop = memoRecallM go
  arrEmpty arc = isampsConstant (arcLen @ElemCount (quantize rate arc)) 0
  sampsEmpty = Samples arrEmpty
  go = \case
    OpEmpty -> pure sampsEmpty
    OpSamp x -> do
      let i = isampsLength x
      pure $
        if i == 0
          then sampsEmpty
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
    OpSlice n (Arc ss se) (Anno rext rsamp) ->
      let sarc' = Arc ss (min se (addDelta ss (extentLen rext)))
          sarcLen' = arcLen sarc'
      in  pure $ Samples $ \(Arc s e) ->
            let arc'@(Arc s' e') = Arc (arcStart sarc' + s) (min e (addDelta s sarcLen'))
                preSamps = arrEmpty (Arc s s')
                postSamps = arrEmpty (Arc e' e)
                bodySamps = runSamples rsamp arc'
                samps = isampsConcat [preSamps, bodySamps, postSamps]
            in  isampsReplicate (fromIntegral n) samps
    OpShift c r -> pure (Samples (\arc -> runSamples (annoVal r) (arcShift arc c)))
    OpConcat rs ->
      let (tot, subArcs) = foldl' (\(t, a) (Anno l g) -> let e = addDelta t (extentLen l) in (e, a :|> (Arc t e, g))) (0, Empty) rs
          whole = Arc 0 tot
      in  pure $ Samples $ \arc@(Arc s e) ->
            let elemLen = arcLen @ElemCount (quantize rate arc)
            in  case arcIntersect arc whole of
                  Nothing -> isampsConstant elemLen 0
                  Just filtArc@(Arc fs fe) ->
                    let preSamps = arrEmpty (Arc s fs)
                        postSamps = arrEmpty (Arc fe e)
                        -- Go through (len, gen) pairs of subArcs to find which overlap with filtArc.
                        -- For each overlap, generate samples, then concatenate them all together
                        gen !samps = \case
                          Empty -> samps
                          (subArc, subGen) :<| rest -> do
                            case arcOverlap filtArc subArc of
                              OverlapLt -> gen samps rest
                              OverlapEq narrowArc ->
                                let subSamps = runSamples subGen narrowArc
                                in  gen (samps :|> subSamps) rest
                              OverlapGt -> samps
                        genSamps = gen Empty subArcs
                    in  isampsConcat (toList (preSamps :<| (genSamps :|> postSamps)))
    OpMerge rs -> pure (Samples (\arc -> isampsMix (fmap ((`runSamples` arc) . annoVal) (toList rs))))
    OpRef n -> lift (onRef n)

opRenderSimple :: Rate -> Op n -> Either n InternalSamples
opRenderSimple rate op = do
  op' <- opAnnoExtentSingle rate op
  samps <- opRender rate Left op'
  pure (maybe isampsEmpty (runSamples samps) (extentPosArc (memoKey op')))

-- data ExSamps n = ExSamps
--   { esLen :: !Len
--   , esGen :: !(Extent -> InternalSamples)
--   }

-- opRender :: forall n. (n -> Either n (ExSamps n)) -> Op n -> Either n (ExSamps n)
-- opRender onRef = goTop where
--   goTop = cata go
--   exEmpty = ExSamps ExLenEmpty (\(Extent _ l) -> isampsConstant l 0)
--   go = \case
--     OpEmpty -> pure exEmpty
--     OpSamp s -> do
--       let i = isampsLength s
--       if i == 0
--         then pure exEmpty
--         else do
--           let meas = ExLenFixed i
--               gen (Extent o l) =
--                 let l' = max 0 (min l (i - o))
--                 in if l' == 0
--                   then isampsConstant l 0
--                   else
--                     let s' = isampsTrim o l' s
--                     in if l' == l
--                       then s'
--                       else isampsConcat [s', isampsConstant (l - l') 0]
--           pure (ExSamps meas gen)
--     OpBound b r -> do
--       ExSamps rmeas rgen <- r
--       let meas = case rmeas of
--             ExLenEmpty -> ExLenEmpty
--             _ -> ExLenFixed b
--           gen (Extent o l) =
--             let l' = case rmeas of
--                   ExLenEmpty -> 0
--                   ExLenRepeat -> b
--                   ExLenFixed i -> max 0 (min i (b - o))
--             in if l' == 0
--               then isampsConstant l 0
--               else
--                 let z = rgen (Extent o l')
--                 in if l' == l
--                   then z
--                   else isampsConcat [z, isampsConstant (l - l') 0]
--       pure (ExSamps meas gen)
--     -- OpSkip c r -> do
--     --   ExSamps rmeas rgen <- r
--     --   let meas = fmap (max 0 . subtract c) rmeas
--     --       gen (Extent o l) = rgen (Extent (o + c) l)
--     --   pure $ ExSamps meas gen
--     -- OpRepeat r -> do
--     --   ExSamps rmeas rgen <- r
--     --   let meas = case rmeas of
--     --         Just 0 -> Just 0
--     --         _ -> Nothing
--     --       gen ex@(Extent o l) = undefined
--     --         -- let m = rmeas ex
--     --         -- in if m == 0
--     --         --   then isampsConstant l 0
--     --         --   else
--     --         --     let z = rgen (Extent o l')
--     --   pure (ExSamps meas gen)
--     --   es@(ExSamps mi f) <- r
--     --   case mi of
--     --     Nothing -> pure es
--     --     Just i ->
--     --       if i <= 0
--     --         then go OpEmpty
--     --         else pure $ ExSamps Nothing $ \(Extent o l) ->
--     --           error "TODO"
--     -- OpReplicate n r -> do
--     --   ExSamps f <- r
--     --   pure $ ExSamps $ \(Extent o l) ->
--     --     error "TODO"
--     OpConcat rs -> do
--       qs <- sequence rs
--       let meas ex@(Extent _ l) = min l (sum (fmap (`esMeas` ex) (toList qs)))
--           gen (Extent o l) = undefined
--       pure (ExSamps meas gen)
--     OpMerge rs -> do
--       qs <- sequence rs
--       let meas ex@(Extent _ l) = min l (maximum (fmap (`esMeas` ex) (toList qs)))
--           gen (Extent o l) = undefined
--       pure (ExSamps meas gen)
--     OpRef n -> onRef n
--     _ -> undefined

-- data Op2F n r
--   = Op2Empty
--   | Op2Samp !Extent !InternalSamples
--   | Op2Replicate !Int r
--   | Op2Concat !(Seq r)
--   | Op2Merge !(Seq r)
--   | Op2Ref !Extent !n
--   deriving stock (Eq, Show, Functor, Foldable, Traversable)

-- data ValErr n
--   = -- | Expected length, actual sum of child lengths
--     ValErrChildLen !ElemCount !ElemCount
--   | -- | Expected length, actual extent length
--     ValErrExtentLen !ElemCount !ElemCount
--   | -- | Non-zero length for Op2Empty
--     ValErrEmptyLen !ElemCount
--   | -- | Negative offset in extent
--     ValErrNegativeExtentOffset !ElemCount
--   | -- | Negative length in extent
--     ValErrNegativeExtentLength !ElemCount
--   | -- | Non-positive number of repetitions
--     ValErrNonPositiveReps !Int
--   | -- | No children in Op2Concat or Op2Merge
--     ValErrNoChildren
--   | -- | Reference not found
--     ValErrRef !n
--   deriving stock (Eq, Show)

-- op2Validate :: (n -> Bool) -> Op2Anno n -> Either (ValErr n) ()
-- op2Validate onRef = runExcept . go
--  where
--   go (MemoP totLen op) = case op of
--     Op2Empty ->
--       unless (totLen == 0) $ throwError (ValErrEmptyLen totLen)
--     Op2Samp (Extent off len) _ -> do
--       unless (off >= 0) $ throwError (ValErrNegativeExtentOffset off)
--       unless (len >= 0) $ throwError (ValErrNegativeExtentLength len)
--       unless (totLen == len) $ throwError (ValErrExtentLen totLen len)
--     Op2Replicate n r -> do
--       go r
--       -- TODO throw error on single rep
--       unless (n > 0) $ throwError (ValErrNonPositiveReps n)
--       let MemoP rLen _ = r
--           childSum = rLen * ElemCount n
--       unless (childSum <= totLen) $ throwError (ValErrChildLen totLen childSum)
--     Op2Concat rs -> do
--       traverse_ go rs
--       when (Seq.null rs) $ throwError ValErrNoChildren
--       let childSum = sum (fmap memoKey rs)
--       unless (childSum <= totLen) $ throwError (ValErrChildLen totLen childSum)
--     Op2Merge rs -> do
--       traverse_ go rs
--       when (Seq.null rs) $ throwError ValErrNoChildren
--       let childSum = maximum (fmap memoKey rs)
--       unless (childSum <= totLen) $ throwError (ValErrChildLen totLen childSum)
--     Op2Ref (Extent off len) n -> do
--       unless (off >= 0) $ throwError (ValErrNegativeExtentOffset off)
--       unless (len >= 0) $ throwError (ValErrNegativeExtentLength len)
--       unless (totLen == len) $ throwError (ValErrExtentLen totLen len)
--       unless (onRef n) $ throwError (ValErrRef n)

-- type Op2Anno n = Memo (Op2F n) ElemCount

-- op2Empty :: Op2Anno n
-- op2Empty = MemoP 0 Op2Empty

-- op2Null :: Op2Anno n -> Bool
-- op2Null = \case
--   MemoP _ Op2Empty -> True
--   MemoP len _ -> len <= 0

-- op2NullNorm :: Op2Anno n -> Op2Anno n
-- op2NullNorm = \case
--   an@(MemoP _ Op2Empty) -> an
--   MemoP len _ | len <= 0 -> op2Empty
--   an -> an

-- type LenM n = ReaderT (Map n (Maybe ElemCount)) (State (Map n Extent))

-- runLenM :: Map n (Maybe ElemCount) -> Map n Extent -> LenM n a -> (a, Map n Extent)
-- runLenM r s m = runState (runReaderT m r) s

-- -- Given available length and original definition,
-- -- Returns (calculated length, annotated translation).
-- -- Properties:
-- -- 1. If it's possible to infer a length, it will be GTE the calculated length.
-- -- 2. The calculated length will be LTE available length.
-- opAnnoLen :: (Ord n) => Extent -> Op n -> LenM n (Op2Anno n)
-- opAnnoLen = go
--  where
--   go ext op =
--     if extNull ext
--       then pure op2Empty
--       else fmap op2NullNorm (goOp ext op)
--   goOp haveExt@(Extent haveOff haveLen) (Fix opf) = case opf of
--     OpEmpty -> pure op2Empty
--     OpSamp s ->
--       let len' = min haveLen (isampsLength s - haveOff)
--       in  pure (MemoP len' (Op2Samp (Extent haveOff len') s))
--     OpBound include r -> do
--       let len' = min haveLen include
--       r' <- go (Extent haveOff len') r
--       let MemoP len'' f = r'
--       pure (MemoP len' (if len'' == len' then f else Op2Concat (r' :<| Empty)))
--     OpSkip exclude r ->
--       let len' = max 0 (haveLen - exclude)
--       in  go (Extent (haveOff + exclude) len') r
--     OpRepeat r -> do
--       r' <- go (Extent 0 (haveLen + haveOff)) r
--       let MemoP len' _ = r'
--       if len' <= 0
--         then pure op2Empty
--         else do
--           let preOff = mod haveOff len'
--               preLen = if preOff > 0 then len' - preOff else 0
--               (numBody, postLen) = divMod (haveLen - preLen) len'
--               bodyLen = len' * numBody
--               body = if bodyLen > 0 then Just (MemoP bodyLen (Op2Replicate (unElemCount numBody) r')) else Nothing
--           pre <-
--             if preLen > 0
--               then fmap (\x -> if op2Null x then Nothing else Just x) (go (Extent preOff preLen) r)
--               else pure Nothing
--           post <-
--             if postLen > 0
--               then fmap (\x -> if op2Null x then Nothing else Just x) (go (Extent 0 postLen) r)
--               else pure Nothing
--           let qs = maybe Empty Seq.singleton pre <> maybe Empty Seq.singleton body <> maybe Empty Seq.singleton post
--               qsLen = maybe 0 memoKey pre + bodyLen + maybe 0 memoKey post
--           case qs of
--             Empty -> pure op2Empty
--             q :<| Empty -> pure q
--             _ -> pure (MemoP qsLen (Op2Concat qs))
--     OpReplicate n r -> do
--       r' <- go (Extent 0 (haveLen + haveOff)) r
--       let MemoP len' _ = r'
--       if len' <= 0
--         then pure op2Empty
--         else do
--           let (ElemCount numPre, preOff) = divMod haveOff len'
--               preLen = if preOff > 0 then len' - preOff else 0
--               numFull = unElemCount (div (haveLen + haveOff) len')
--               numBody = min n numFull - numPre
--               bodyLen = len' * ElemCount numBody
--               postLen = if numBody < n then haveLen - (preLen + bodyLen) else 0
--               body = if bodyLen > 0 then Just (MemoP bodyLen (Op2Replicate numBody r')) else Nothing
--           pre <-
--             if preLen > 0
--               then fmap (\x -> if op2Null x then Nothing else Just x) (go (Extent preOff preLen) r)
--               else pure Nothing
--           post <-
--             if postLen > 0
--               then fmap (\x -> if op2Null x then Nothing else Just x) (go (Extent 0 postLen) r)
--               else pure Nothing
--           let qs = maybe Empty Seq.singleton pre <> maybe Empty Seq.singleton body <> maybe Empty Seq.singleton post
--               qsLen = maybe 0 memoKey pre + bodyLen + maybe 0 memoKey post
--           case qs of
--             Empty -> pure op2Empty
--             q :<| Empty -> pure q
--             _ -> pure (MemoP qsLen (Op2Concat qs))
--     OpConcat rs -> do
--       let process !tot !qs = \case
--             Empty -> pure (MemoP tot (Op2Concat qs))
--             p :<| ps -> do
--               let left = haveLen - tot
--               if left > 0
--                 then do
--                   q <- go (Extent 0 left) p
--                   let MemoP len _ = q
--                   process (tot + len) (qs :|> q) ps
--                 else pure (MemoP tot (Op2Concat qs))
--       if haveOff == 0
--         then process 0 Empty rs
--         else do
--           -- Find the subtree containing our offset
--           let findOffset !tot = \case
--                 Empty -> pure (0, Nothing, Empty)
--                 p :<| ps -> do
--                   -- Calculate length of subtree without offset
--                   q <- go (Extent 0 (haveLen + haveOff - tot)) p
--                   let MemoP len _ = q
--                   if tot + len <= haveOff
--                     then findOffset (tot + len) ps
--                     else do
--                       -- Found the subtree containing our offset
--                       let subOffset = haveOff - tot
--                       q' <- go (Extent subOffset haveLen) p
--                       let MemoP len' _ = q'
--                       pure (len', Just q', ps)
--           (tot, partial, rest) <- findOffset 0 rs
--           process tot (maybe Empty Seq.singleton partial) rest
--     OpMerge rs -> do
--       ps <- fmap (Seq.filter (not . op2Null)) (traverse (go haveExt) rs)
--       case ps of
--         Empty -> pure op2Empty
--         p :<| Empty -> pure p
--         _ -> do
--           let maxLen = maximum (fmap memoKey (toList ps))
--           pure (MemoP maxLen (Op2Merge ps))
--     OpRef n -> do
--       mx <- asks (join . Map.lookup n)
--       len' <- case mx of
--         Just x -> pure (max 0 (min x (haveLen + haveOff) - haveOff))
--         Nothing -> haveLen <$ modify' (Map.alter (Just . maybe haveExt (<> haveExt)) n)
--       pure (MemoP len' (Op2Ref (Extent haveOff len') n))

-- data AnnoErr n
--   = AnnoErrTopo !(TopoErr n)
--   | AnnoErrOrphan !n
--   deriving stock (Eq, Ord, Show)

-- instance (Show n, Typeable n) => Exception (AnnoErr n)

-- data AnnoSt n = AnnoSt
--   { asSpaceNeed :: !(Map n Extent)
--   , asAnnoOps :: !(Map n (Op2Anno n))
--   }

-- type AnnoM n = ReaderT (Map n (Maybe ElemCount)) (StateT (AnnoSt n) (Except (AnnoErr n)))

-- execAnnoM
--   :: Map n (Maybe ElemCount)
--   -> Map n Extent
--   -> Map n (Op2Anno n)
--   -> AnnoM n ()
--   -> Either (AnnoErr n) (Map n (Op2Anno n))
-- execAnnoM r s1 s2 m = fmap asAnnoOps (runExcept (execStateT (runReaderT m r) (AnnoSt s1 s2)))

-- lenToAnnoM :: LenM n a -> AnnoM n a
-- lenToAnnoM m = do
--   r <- ask
--   s <- gets asSpaceNeed
--   let (a, s') = runLenM r s m
--   modify' (\as -> as {asSpaceNeed = s'})
--   pure a

-- -- | Annotate all ops with lengths, inferring bottom-up then top-down.
-- opAnnoLenTopo
--   :: forall n. (Ord n) => Map n (Op n) -> Map n (Maybe ElemCount) -> Either (AnnoErr n) (Map n (Op2Anno n))
-- opAnnoLenTopo m n = execAnnoM n Map.empty Map.empty $ do
--   let calc k val = do
--         let op = m Map.! k
--         anno <- lenToAnnoM (opAnnoLen val op)
--         modify' (\as -> as {asAnnoOps = Map.insert k anno (asAnnoOps as)})
--   -- Calculate reverse topo order (top down)
--   ks <-
--     either
--       (throwError . AnnoErrTopo)
--       (pure . Seq.reverse)
--       (topoSort (fmap opRefs . flip Map.lookup m) (Map.keys m))
--   -- First round - if it has an inferred len, annotate.
--   for_ ks $ \k -> do
--     minf <- asks (join . Map.lookup k)
--     for_ minf (calc k . Extent 0)
--   -- Second round - if found a new len, annotate. Otherwise fail.
--   for_ ks $ \k -> do
--     minf <- asks (join . Map.lookup k)
--     case minf of
--       Just _ -> pure ()
--       Nothing -> do
--         mfound <- gets (Map.lookup k . asSpaceNeed)
--         case mfound of
--           Nothing -> throwError (AnnoErrOrphan k)
--           Just found -> calc k found

-- data OpEnv n = OpEnv
--   { oeDefs :: !(Map n (Op2Anno n))
--   , oeFutVar :: !(MVar (Map n (IVar (PrimArray Int32))))
--   , oeOff :: !ElemCount
--   , oeBufVar :: !(MVar (MutablePrimArray RealWorld Int32))
--   }
--   deriving stock (Eq)

-- type OpM n = ReadPar (OpEnv n)

-- execOpM :: Map n (Op2Anno n) -> ElemCount -> OpM n () -> IO (PrimArray Int32)
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
