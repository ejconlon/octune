module Octune.CodeGen.OpGen where

import Dahdit.Audio.Wav.Simple (WAVESamples)
import Octune.Types.Core (Core)
import Octune.Types.Env (Env)

genSamples :: Env Core -> Int -> Int -> Bool -> Core -> WAVESamples
genSamples _env _bpm _frameRate _memoize = error "TODO"
