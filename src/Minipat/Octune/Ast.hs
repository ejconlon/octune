module Minipat.Octune.Ast where

import Data.Sequence (Seq)
import Data.Text (Text)

data Waveform
  = Square
  | Sawtooth
  | Triangle
  deriving stock (Eq, Ord, Show)

data Fun
  = -- | Sequence of samples
    FunSequ
  | -- | Layering samples
    FunMerge
  | -- | Repeating sequence of samples
    FunRepeat !Rational
  | -- | Set waveform to use
    FunWaveform !Waveform
  | -- | Set amplitude of generated samples
    FunVolume !Rational
  | -- | Take the subsection of a sequence of samples between the ends of the given beats
    -- Note: the end of the 0th beat is the beginning of the 1st beat
    FunSlice !Beats !Beats
  deriving stock (Eq, Ord, Show)

type ModName = Seq Text

data QualName = QualName
  { qnMod :: !ModName
  , qnVar :: !Text
  }
  deriving stock (Eq, Ord, Show)

data File r = File
  { fileMod :: !ModName
  , fileDecls :: !(Seq r)
  }
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

data Decl r = Decl
  { declName :: !Text
  , declExp :: !r
  }
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

data ExpF r
  = ExpVar !QualName
  | ExpNote !Note
  | ExpApp !Fun !(Seq r)
  | ExpCheck !Beats r
  | ExpBpm !Rational r
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

data Letter
  = C
  | D
  | E
  | F
  | G
  | A
  | B
  deriving stock (Eq, Ord, Show)

data Accidental
  = Flat
  | Sharp
  deriving stock (Eq, Ord, Show)

type Octave = Int

data Percussion
  = Snare
  | Clap
  deriving stock (Eq, Ord, Show)

data Sound
  = Pitch !Letter !(Maybe Accidental) !Octave
  | Drum !Percussion
  | Rest
  deriving stock (Eq, Ord, Show)

data NoteModifier
  = Detached
  | Staccato
  deriving stock (Eq, Ord, Show)

type Beats = Rational

data Note = Note !(Seq NoteModifier) !Beats !Sound
  deriving stock (Eq, Ord, Show)
