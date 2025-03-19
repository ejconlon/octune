module Octune.Player where

import Control.Concurrent (threadDelay)
import Control.Exception (finally)
import Control.Monad (void, when)
import Dahdit.Audio.Common (rethrow)
import Dahdit.Audio.Wav.Simple qualified as DAWS
import Dahdit.Iface (encode)
import Dahdit.Sizes (ElemCount (..))
import Data.Sounds (InternalSamples, dumpSamples, isampsLength)
import Sound.ProteaAudio qualified as P

play :: InternalSamples -> IO ()
play samps = do
  let sr = 48000
      micros = div (1000000 * unElemCount (isampsLength samps)) sr
  simpleWav <- dumpSamples samps
  complexWav <- rethrow (DAWS.toComplex simpleWav)
  strWav <- encode complexWav
  let act = do
        ok <- P.initAudio 1 sr 512
        when ok $ do
          psamp <- P.sampleFromMemoryWav strWav 1
          sound <- P.soundPlay psamp 1 1 0 1
          threadDelay micros
          void (P.soundStop sound)
      cleanup = do
        P.soundStopAll
        P.finishAudio
  finally act cleanup
