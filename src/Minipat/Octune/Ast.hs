module Minipat.Octune.Ast where

import Bowtie (Fix)
import Data.Sequence (Seq)
import Data.Text (Text)

data Waveform
  = Square
  | Sawtooth
  | Triangle
  deriving (Eq, Ord, Show)

data Fun
  = -- Sequence of samples
    FunSeq
  | -- Layering samples
    FunMerge
  | -- Repeating sequence of samples
    FunRepeat !Int
  | -- Set waveform to use
    FunWaveform !Waveform
  | -- Set amplitude of generated samples
    FunVolume !Rational
  | -- Take the subsection of a sequence of samples between
    --   the ends of the given beats
    -- Note: the end of the 0th beat is the beginning of the 1st beat
    FunSlice !Beats !Beats
  deriving (Eq, Ord, Show)

data QualName = QualName
  { qnModule :: !(Seq Text)
  , qnVar :: !Text
  }
  deriving (Eq, Ord, Show)

data AstF r
  = AstFile !(Seq Text) !(Seq r)
  | AstDecl !Text r
  | AstSong !Rational r
  | AstVar !QualName
  | AstNote !Note
  | AstApp !Fun !(Seq r)
  deriving (Eq, Ord, Show)

type Ast = Fix AstF

data CoreF r
  = CoreSong !Rational r
  | CoreVar !QualName
  | CoreNote !Note
  | CoreApp !Fun !(Seq r)
  deriving (Eq, Ord, Show)

type Core = Fix CoreF

data Letter
  = C
  | D
  | E
  | F
  | G
  | A
  | B
  deriving (Eq, Ord, Show)

data Accidental
  = Flat
  | Sharp
  deriving (Eq, Ord, Show)

type Octave = Int

data Percussion
  = Snare
  | Clap
  deriving (Eq, Ord, Show)

data Sound
  = Pitch !Letter !(Maybe Accidental) !Octave
  | Drum !Percussion
  | Rest
  deriving (Eq, Ord, Show)

-- TODO: more modifiers
data NoteModifier
  = Detached
  | Staccato
  deriving (Eq, Ord, Show)

type Beats = Rational

data Note = Note !(Seq NoteModifier) !Beats !Sound
  deriving (Eq, Ord, Show)
