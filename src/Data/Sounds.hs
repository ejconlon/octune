module Data.Sounds where

import Control.DeepSeq (NFData (..))
import Control.Monad (unless, (>=>))
import Dahdit.Audio.Binary (QuietLiftedArray (..))
import Dahdit.Audio.Wav.Simple (WAVE (..), WAVEHeader (..), WAVESamples (..), getWAVEFile, putWAVEFile)
import Dahdit.LiftedPrimArray
  ( LiftedPrimArray (..)
  , cloneLiftedPrimArray
  , concatLiftedPrimArray
  , emptyLiftedPrimArray
  , generateLiftedPrimArray
  , indexLiftedPrimArray
  , lengthLiftedPrimArray
  , liftedPrimArrayFromList
  , mapLiftedPrimArray
  , mergeLiftedPrimArray
  , replicateLiftedPrimArray
  )
import Dahdit.Sizes (ElemCount (..))
import Data.Foldable (for_)
import Data.Int (Int32)
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
