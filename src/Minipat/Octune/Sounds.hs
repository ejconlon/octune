module Minipat.Octune.Sounds where

import Control.Monad (unless, (>=>))
import Dahdit.Audio.Wav.Simple (WAVE (..), WAVEHeader (..), getWAVEFile, putWAVEFile)
import Dahdit.Sizes (ElemCount (..))
import Data.Foldable (for_)
import Minipat.Octune.InternalSamples (InternalSamples (..), isampsFromWave, isampsLength, isampsToWave)
import Paths_octune (getDataFileName)
import System.Directory (doesFileExist)
import System.IO.Unsafe (unsafePerformIO)

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
