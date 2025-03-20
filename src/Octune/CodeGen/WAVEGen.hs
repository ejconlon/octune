{-# LANGUAGE OverloadedStrings #-}

module Octune.CodeGen.WAVEGen where

import Dahdit.Audio.Wav.Simple (WAVE (..), WAVEHeader (..))
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Octune.CodeGen.NewSamplesGen (genSamples)
import Octune.Types.AST (QualifiedName (..))
import Octune.Types.Core (Core (..))
import Octune.Types.Env (Env)

-- Number of frames per second
frameRate :: Int
frameRate = 48000

genWAVE :: Env Core -> [Text] -> Either Text WAVE
genWAVE env mainModule =
  case Map.lookup (QualName mainModule "main") env of
    Just (CoreSong bpm coreExpr) ->
      -- TODO: provide memoization flag based on something
      pure $ WAVE header (genSamples env bpm frameRate False coreExpr)
    Just _ ->
      Left "`main` must be a song expression"
    Nothing ->
      Left "Program must contain `main`"
 where
  header :: WAVEHeader
  header =
    WAVEHeader
      { waveNumChannels = 1
      , waveFrameRate = frameRate
      , waveBitsPerSample = 16
      , waveFrames = Nothing
      }
