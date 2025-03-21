module Octune.CodeGen.OpGen where

import Control.Lens (foldlOf', traversed)
import Control.Monad (join, (>=>))
import Control.Monad.Par (Par, parMapM, runPar)
import Control.Monad.Par.Class (ParFuture (..))
import Control.Monad.Par.IO (IVar)
import Control.Monad.Par.Scheds.TraceInternal qualified as IVar
import Control.Monad.Reader (MonadReader, ReaderT (..), asks, local)
import Dahdit (ElemCount (..))
import Dahdit.Audio.Wav.Simple (WAVESamples)
import Data.Bits (shiftL)
import Data.Int (Int32)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Sounds
  ( InternalSamples
  , SampleStream (..)
  , clapSamples
  , isampsConcat
  , isampsConstant
  , isampsEmpty
  , isampsFill
  , isampsFromList
  , isampsLength
  , isampsMap
  , isampsMix
  , isampsReplicate
  , isampsToWave
  , isampsTrim
  , snareSamples
  , streamToIsamps
  )
import Octune.Types.AST (LineFun (..), Waveform (..))
import Octune.Types.Core (Core (..))
import Octune.Types.Env (Env)
import Octune.Types.Note
  ( Accidental (..)
  , Beats
  , Letter (..)
  , Note (..)
  , NoteModifier (..)
  , Octave
  , Percussion (..)
  , Sound (..)
  )

-- Default amplitude of a wave
amplitude :: Int32
amplitude = 1 `shiftL` 27 + 1 `shiftL` 26

-- Multiplier for frequency to go up a semitone
semitoneFreqMultiplier :: Rational
semitoneFreqMultiplier = 1.05946309435929

multRat :: (Integral a) => Rational -> a -> a
multRat rat = round . (* rat) . toRational

takeUntilOrN :: (a -> Bool) -> Int -> [a] -> ([a], [a])
takeUntilOrN _ _ [] = ([], [])
takeUntilOrN p n lst@(x : xs)
  | n <= 0 = ([], lst)
  | p x = ([x], xs)
  | otherwise = let (ls, rs) = takeUntilOrN p (n - 1) xs in (x : ls, rs)

beatsToNumFrames :: Int -> Int -> Beats -> Int
beatsToNumFrames bpm frameRate beats =
  round (secondsPerBeat * toRational frameRate)
 where
  secondsPerBeat = (beats / toRational bpm) * 60

data GenEnv = GenEnv
  { geEnv :: !(Env Core)
  , geBpm :: !Int
  , geFrameRate :: !Int
  , geWaveform :: !(Maybe Waveform)
  }
  deriving stock (Eq, Show)

newtype GenM a = GenM {unGenM :: ReaderT GenEnv Par a}
  deriving newtype (Functor, Applicative, Monad, MonadReader GenEnv)

instance ParFuture IVar GenM where
  spawn_ act = GenM (ReaderT (runGenM act >=> IVar.newFull_))
  get iv = GenM (ReaderT (const (IVar.get iv)))

runGenM :: GenM a -> GenEnv -> Par a
runGenM = runReaderT . unGenM

modifySamplesVolume :: Rational -> InternalSamples -> InternalSamples
modifySamplesVolume multiplier = isampsMap (multRat multiplier)

subsectionOfSamples
  :: Beats
  -> Beats
  -> InternalSamples
  -> GenM InternalSamples
subsectionOfSamples beg end samps = do
  bpm <- asks geBpm
  frameRate <- asks geFrameRate
  let beginningFrames = beatsToNumFrames bpm frameRate beg
      durationFrames = beatsToNumFrames bpm frameRate (end - beg)
  pure (isampsTrim (ElemCount beginningFrames) (ElemCount durationFrames) samps)

waveformOrDefault :: Maybe Waveform -> Waveform
waveformOrDefault = fromMaybe Square

genSamples :: Env Core -> Int -> Int -> Bool -> Core -> WAVESamples
genSamples env bpm frameRate _memoize = isampsToWave . runPar . flip runGenM (GenEnv env bpm frameRate Nothing) . genCore

genCore :: Core -> GenM InternalSamples
genCore = \case
  CoreVar vName -> asks geEnv >>= \env -> genCore (env Map.! vName)
  CoreNote note -> noteToSamples note
  CoreApp lineFun lineArgs -> applyLineFun lineFun lineArgs
  CoreSong {} -> error "Should not be called on CoreSongs"

applyLineFun :: LineFun -> [Core] -> GenM InternalSamples
applyLineFun = \case
  Seq -> fmap isampsConcat . goListChunked
  -- Merge's arguments will usually be around the same length,
  --   so we process them in parallel without chunking
  Merge -> fmap isampsMix . parMapM genCore
  Repeat n -> fmap (isampsReplicate n . isampsConcat) . goListChunked
  UsingWaveform setWaveform ->
    local (\env -> env {geWaveform = Just setWaveform}) . fmap isampsConcat . goListChunked
  Volume rat -> fmap (modifySamplesVolume rat . isampsConcat) . goListChunked
  Subsection beg end -> goListChunked >=> subsectionOfSamples beg end . isampsConcat

goListChunked :: [Core] -> GenM [InternalSamples]
goListChunked =
  fmap join
    . parMapM (traverse genCore)
    . chunk

maxChunkSize :: Int
maxChunkSize = 24

chunk :: [Core] -> [[Core]]
chunk [] = []
chunk es@(CoreNote {} : _) = notes : chunk rest
 where
  (notes, rest) = takeUntilOrN notNote maxChunkSize es
chunk (e : es) = [e] : chunk es

notNote :: Core -> Bool
notNote CoreNote {} = False
notNote _ = True

noteToSamples :: Note -> GenM InternalSamples
noteToSamples (Note noteMods beats sound) = do
  durationFrames <- asks (\env -> beatsToNumFrames (geBpm env) (geFrameRate env) beats)
  wave <- soundWave sound
  let unmodifiedSamples = streamToIsamps 0 (ElemCount durationFrames) wave
  pure (foldlOf' traversed (flip applyModifier) unmodifiedSamples noteMods)

applyModifier :: NoteModifier -> InternalSamples -> InternalSamples
applyModifier Detached samples =
  -- Make the last 20% of the note silent
  let len = isampsLength samples
      off = div (4 * len) 5
  in  isampsFill off (len - off) 0 samples
applyModifier Staccato samples =
  -- Make the last 75% of the note silent
  let len = isampsLength samples
      off = div (isampsLength samples) 4
  in  isampsFill off (len - off) 0 samples

soundWave :: Sound -> GenM SampleStream
soundWave = \case
  Rest -> pure $ SampleStream isampsEmpty isampsEmpty
  Drum percussion ->
    -- TODO: adjust based on framerate
    pure $ case percussion of
      Snare -> SampleStream snareSamples isampsEmpty
      Clap -> SampleStream clapSamples isampsEmpty
  Pitch letter accidental octave -> do
    frameRate <- asks geFrameRate
    let intAmplitude = fromIntegral amplitude
        wavelenFrames = wavelenFramesOf frameRate letter accidental octave
        (h1, h2) = splitHalf wavelenFrames
        (q1, q2) = splitHalf h1
        (q3, q4) = splitHalf h2
    -- upSlope is (2*amplitude - 0) / (1/4 * wavelenFrames);
    -- downSlope is -upSlope
    let upSlope :: Rational
        upSlope = toRational (8 * intAmplitude) / toRational wavelenFrames
    -- line going from (0, 0) to (1/4 * wavelenFrames, 2 * amplitude)
    let baseLineEqn :: Int -> Int
        baseLineEqn = multRat upSlope
    wf <- asks (waveformOrDefault . geWaveform)
    let samps = case wf of
          Square ->
            isampsConcat
              [ isampsConstant (ElemCount h1) amplitude
              , isampsConstant (ElemCount h2) (-amplitude)
              ]
          Sawtooth -> do
            -- Note: `2*amplitude` max amplitude to reach the same energy
            --       as a square wave with amplitude `amplitude`
            -- slope is (2*amplitude - 2*(-amplitude)) / wavelenFrames
            let slope :: Rational
                slope = toRational (4 * intAmplitude) / toRational wavelenFrames
            -- wave looks like ////
            let lineEqn :: Int -> Int
                lineEqn i = multRat slope i - 2 * intAmplitude
            samplesFromEquation lineEqn [0 .. wavelenFrames - 1]
          Triangle -> do
            -- Note: `2*amplitude` max amplitude to reach the same energy
            --       as a square wave with amplitude `amplitude`
            isampsConcat
              [ samplesFromEquation
                  baseLineEqn
                  [0 .. q1 - 1]
              , samplesFromEquation
                  ((+ 2 * intAmplitude) . negate . baseLineEqn)
                  [0 .. (q2 + q3) - 1]
              , samplesFromEquation
                  (subtract (2 * intAmplitude) . baseLineEqn)
                  [0 .. q4 - 1]
              ]
    pure (SampleStream isampsEmpty samps)

splitHalf :: Int -> (Int, Int)
splitHalf n =
  let firstHalf = n `div` 2
      secondHalf = n - firstHalf
  in  (firstHalf, secondHalf)

samplesFromEquation :: (Int -> Int) -> [Int] -> InternalSamples
samplesFromEquation eqn = isampsFromList . fmap (fromIntegral . eqn)

-- Frequency of `Sound letter accidental 4`
-- Obtained from https://en.wikipedia.org/wiki/Piano_key_frequencies
-- We assume most notes will be close to octave 4 to optimize
-- frequency calculation below
baseFrequencyOf :: Letter -> Maybe Accidental -> Rational
baseFrequencyOf letter accidental =
  case (letter, accidental) of
    (C, Just Flat) -> 246.9417
    (C, Nothing) -> 261.6256
    (C, Just Sharp) -> 277.1826
    (D, Just Flat) -> 277.1826
    (D, Nothing) -> 293.6648
    (D, Just Sharp) -> 311.1270
    (E, Just Flat) -> 311.1270
    (E, Nothing) -> 329.6276
    (E, Just Sharp) -> 349.2282
    (F, Just Flat) -> 329.6276
    (F, Nothing) -> 349.2282
    (F, Just Sharp) -> 369.9944
    (G, Just Flat) -> 369.9944
    (G, Nothing) -> 391.9954
    (G, Just Sharp) -> 415.3047
    (A, Just Flat) -> 415.3047
    (A, Nothing) -> 440.0000
    (A, Just Sharp) -> 466.1638
    (B, Just Flat) -> 466.1638
    (B, Nothing) -> 493.8833
    (B, Just Sharp) -> 523.2511

-- Note: `octave` should be valid (0 <= octave <= 8) from parsing
frequencyOf :: Letter -> Maybe Accidental -> Octave -> Rational
frequencyOf letter accidental octave = baseFrequencyOf letter accidental * (2 ^^ (octave - 4))

-- frameRate / frequency = wavelength in frames
wavelenFramesOf :: Int -> Letter -> Maybe Accidental -> Octave -> Int
wavelenFramesOf frameRate letter accidental octave = round (toRational frameRate / frequencyOf letter accidental octave)
