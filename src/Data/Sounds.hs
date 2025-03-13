module Data.Sounds where

import Control.Monad (unless)
import Data.Foldable (for_)
import Data.Int (Int32)
import Data.WAVE (WAVE (..), WAVEHeader (..), getWAVEFile, putWAVEFile)
import Paths_octune (getDataFileName)
import System.IO.Unsafe (unsafeDupablePerformIO)

type InternalSamples = [Int32]

loadSamples :: FilePath -> IO InternalSamples
loadSamples path = do
  WAVE header samples <- getWAVEFile path
  unless (waveNumChannels header == 1) (fail "sample wav must be mono")
  pure (concat samples)

loadDataSamples :: String -> InternalSamples
loadDataSamples name = unsafeDupablePerformIO (getDataFileName ("data/" ++ name ++ ".wav") >>= loadSamples)

snareSamples :: InternalSamples
snareSamples = loadDataSamples "snare"

clapSamples :: InternalSamples
clapSamples = loadDataSamples "clap"

dumpSamples :: InternalSamples -> WAVE
dumpSamples rawSamples = WAVE header samples
 where
  header =
    WAVEHeader
      { waveNumChannels = 1
      , waveFrameRate = 48000
      , waveBitsPerSample = 16
      , waveFrames = Just (length rawSamples)
      }
  samples = fmap pure rawSamples

dumpAllSamples :: IO ()
dumpAllSamples = do
  let items = [] :: [(String, InternalSamples)]
  for_ items $ \(name, samps) -> do
    let wav = dumpSamples samps
    putWAVEFile ("data/" ++ name ++ ".wav") wav
