module Data.Sounds where

import Control.DeepSeq (NFData (..))
import Control.Exception (Exception)
import Control.Monad (unless, (>=>))
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (Reader, ReaderT (..), ask, asks, local, runReader)
import Control.Monad.ST.Strict (ST, runST)
import Control.Monad.Trans (lift)
import Dahdit.Audio.Binary (QuietLiftedArray (..))
import Dahdit.Audio.Wav.Simple (WAVE (..), WAVEHeader (..), WAVESamples (..), getWAVEFile, putWAVEFile)
import Dahdit.LiftedPrimArray
  ( LiftedPrimArray (..)
  , MutableLiftedPrimArray (..)
  , cloneLiftedPrimArray
  , concatLiftedPrimArray
  , emptyLiftedPrimArray
  , generateLiftedPrimArray
  , indexLiftedPrimArray
  , lengthLiftedPrimArray
  , liftedPrimArrayFromList
  , mapLiftedPrimArray
  , mergeIntoLiftedPrimArray
  , mergeLiftedPrimArray
  , replicateLiftedPrimArray
  )
import Dahdit.Sizes (ElemCount (..))
import Data.Foldable (for_, toList)
import Data.Int (Int32)
import Data.Primitive.ByteArray (copyByteArray, fillByteArray, newByteArray, sizeofByteArray, unsafeFreezeByteArray)
import Data.Sequence (Seq (..))
import Paths_octune (getDataFileName)
import System.IO.Unsafe (unsafeDupablePerformIO)

newtype InternalSamples = InternalSamples {unInternalSamples :: LiftedPrimArray Int32}
  deriving stock (Eq, Show)

instance NFData InternalSamples where
  rnf = rnf . unLiftedPrimArray . unInternalSamples

isampsEmpty :: InternalSamples
isampsEmpty = InternalSamples emptyLiftedPrimArray

isampsIsNull :: InternalSamples -> Bool
isampsIsNull = (0 ==) . isampsLength

isampsLength :: InternalSamples -> Int
isampsLength = unElemCount . lengthLiftedPrimArray . unInternalSamples

isampsIndex :: InternalSamples -> Int -> Int32
isampsIndex s = indexLiftedPrimArray (unInternalSamples s) . ElemCount

isampsReplicate :: Int -> Int32 -> InternalSamples
isampsReplicate len = InternalSamples . replicateLiftedPrimArray (ElemCount len)

isampsFromList :: [Int32] -> InternalSamples
isampsFromList = InternalSamples . liftedPrimArrayFromList

isampsConcat :: [InternalSamples] -> InternalSamples
isampsConcat = InternalSamples . concatLiftedPrimArray . fmap unInternalSamples

isampsMix :: [InternalSamples] -> InternalSamples
isampsMix = InternalSamples . mergeLiftedPrimArray 0 (+) . fmap unInternalSamples

isampsTrim :: Int -> Int -> InternalSamples -> InternalSamples
isampsTrim off len s = InternalSamples (cloneLiftedPrimArray (unInternalSamples s) (ElemCount off) (ElemCount len))

isampsMap :: (Int32 -> Int32) -> InternalSamples -> InternalSamples
isampsMap f = InternalSamples . mapLiftedPrimArray f . unInternalSamples

isampsFill :: Int -> Int -> Int32 -> InternalSamples -> InternalSamples
isampsFill off len val s =
  let f = indexLiftedPrimArray (unInternalSamples s)
      g pos@(ElemCount ipos) = if ipos >= off && ipos < off + len then val else f pos
  in  InternalSamples (generateLiftedPrimArray (ElemCount (isampsLength s)) g)

isampsToWave :: InternalSamples -> WAVESamples
isampsToWave = WAVESamples . QuietLiftedArray . unInternalSamples

isampsFromWave :: WAVESamples -> InternalSamples
isampsFromWave = InternalSamples . unQuietLiftedArray . unWAVESamples

data SampleStream = SampleStream
  { ssFixed :: !InternalSamples
  , ssRepeated :: !InternalSamples
  }
  deriving stock (Eq, Show)

streamRun :: SampleStream -> Int -> Int32
streamRun (SampleStream fixed repeated) =
  let flen = isampsLength fixed
      rlen = isampsLength repeated
  in  \pos ->
        if
          | pos < 0 -> 0
          | pos < flen ->
              indexLiftedPrimArray (unInternalSamples fixed) (ElemCount pos)
          | rlen == 0 -> 0
          | otherwise ->
              indexLiftedPrimArray (unInternalSamples repeated) (ElemCount (mod (pos - flen) rlen))

streamToIsamps :: Int -> Int -> SampleStream -> InternalSamples
streamToIsamps off len t =
  InternalSamples (generateLiftedPrimArray (ElemCount len) (streamRun t . (off +) . unElemCount))

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
loadDataSamples name = unsafeDupablePerformIO (getDataFileName ("data/" ++ name ++ ".wav") >>= loadSamples)

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
      , waveFrames = Just (isampsLength isamps)
      }

dumpAllSamples :: IO ()
dumpAllSamples = do
  let items = [] :: [(String, InternalSamples)]
  for_ items $ \(name, samps) -> dumpSamples samps >>= putWAVEFile ("data/" ++ name ++ ".wav")

data OpF r
  = OpEmpty
  | OpSamp !InternalSamples
  | OpBound !Int r
  | OpCut !Rational r
  | OpRepeat r
  | OpReplicate !Int r
  | OpConcat !(Seq r)
  | OpMerge !(Seq r)
  deriving stock (Eq, Show, Functor)

newtype Op = Op {unOp :: OpF Op}
  deriving stock (Eq, Show)

cataOp :: (OpF r -> r) -> Op -> r
cataOp f = go
 where
  go = f . fmap go . unOp

opInferLenF :: OpF (Maybe Int) -> Maybe Int
opInferLenF = \case
  OpEmpty -> Nothing
  OpSamp s -> Just (isampsLength s)
  OpBound l _ -> Just l
  OpCut _ r -> r
  OpRepeat _ -> Nothing
  OpReplicate n r -> fmap (n *) r
  OpConcat rs -> fmap sum (sequence rs)
  OpMerge rs -> fmap maximum (sequence rs)

opInferLen :: Op -> Maybe Int
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
opAnnoLenF :: Int -> OpF Op -> (Int, Op2F (Op2Anno Int))
opAnnoLenF i = \case
  OpEmpty ->
    (0, Op2Empty)
  OpSamp s ->
    let l' = min i (sizeofByteArray (unLiftedPrimArray (unInternalSamples s)))
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
        r' = Op2Replicate n (Op2Anno (Anno ml f))
    in  if m == 0
          then
            (i, r')
          else
            let q' = Op2Anno (Anno (ml * n) r')
                (ml', f') = opAnnoLenF m (unOp r)
                q'' = Op2Anno (Anno ml' f')
            in  (ml * n + ml', Op2Concat (q' :<| q'' :<| Empty))
  OpReplicate n r ->
    let l' = div i n
        (ml, f) = opAnnoLenF l' (unOp r)
    in  (ml * n, Op2Replicate n (Op2Anno (Anno ml f)))
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

opAnnoLen :: Int -> Op -> Op2Anno Int
opAnnoLen i (Op f) = let (ml, f') = opAnnoLenF i f in Op2Anno (Anno ml f')

data OpEnv s = OpEnv
  { oeOff :: !Int
  , oeBuf :: !(MutableLiftedPrimArray s Int32)
  }
  deriving stock (Eq)

data OpErr
  = OpErrInfer
  | OpErrOverflow
  deriving stock (Eq, Ord, Show)

instance Exception OpErr

type OpM s = ReaderT (OpEnv s) (ExceptT OpErr (ST s))

liftST :: ST s a -> OpM s a
liftST = lift . lift

execOpM :: Int -> (forall s. OpM s ()) -> Either OpErr (LiftedPrimArray Int32)
execOpM len act = runST $ do
  arr <- newByteArray len
  fillByteArray arr 0 len 0
  let env = OpEnv {oeOff = 0, oeBuf = MutableLiftedPrimArray arr}
  ea <- runExceptT (runReaderT act env)
  case ea of
    Left e -> pure (Left e)
    Right () -> fmap (Right . LiftedPrimArray) (unsafeFreezeByteArray arr)

opToWave :: Op -> Either OpErr WAVESamples
opToWave op =
  case opInferLen op of
    Nothing -> Left OpErrInfer
    Just len -> do
      let an = opAnnoLen len op
      arr <- execOpM len (goOpToWave an)
      Right (WAVESamples (QuietLiftedArray arr))

handleOpSamp :: Int -> InternalSamples -> OpM s ()
handleOpSamp clen s = do
  OpEnv off buf <- ask
  let src = unLiftedPrimArray (unInternalSamples s)
  liftST (copyByteArray (unMutableLiftedPrimArray buf) off src 0 clen)

handleOpReplicate :: Int -> Int -> Op2Anno Int -> OpM s ()
handleOpReplicate clen n r = undefined

handleOpConcat :: Int -> Seq (Op2Anno Int) -> OpM s ()
handleOpConcat clen rs = undefined

handleOpMerge :: Int -> Seq (Op2Anno Int) -> OpM s ()
handleOpMerge clen rs = do
  OpEnv origOff origBuf <- ask
  tempArr <- newByteArray clen
  local (\env -> env {oeOff = 0, oeBuf = MutableLiftedPrimArray tempArr}) $ do
    for_ rs $ \_r -> do
      fillByteArray tempArr 0 clen 0
      tempBuf <- fmap LiftedPrimArray (unsafeFreezeByteArray tempArr)
      mergeIntoLiftedPrimArray (+) origBuf (ElemCount origOff) tempBuf 0 (ElemCount clen)

goOpToWave :: Op2Anno Int -> OpM s ()
goOpToWave (Op2Anno (Anno clen f)) = case f of
  Op2Empty -> pure ()
  Op2Samp s -> handleOpSamp clen s
  Op2Replicate n r -> handleOpReplicate clen n r
  Op2Concat rs -> handleOpConcat clen rs
  Op2Merge rs -> handleOpMerge clen rs
