module Debug where

import Config (Config (..))

cfgDbg, cfgDar, cfgSil :: Config
cfgDbg = Config (Just "out/debug") False 1 ["samples/debug.otn"]
cfgDar = Config (Just "out/debug") False 1 ["samples/darude.otn"]
cfgSil =
  Config
    (Just "out/silhouette")
    False
    1
    ( fmap
        (\x -> "samples/" ++ x ++ ".otn")
        ["Bridge", "Chorus", "Common", "Intro", "Outro", "Prechorus", "Silhouette", "Verse"]
    )
