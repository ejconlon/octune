module Minipat.Octune.Ast where

import Bowtie.Anno (Anno)
import Bowtie.Memo (Memo)
import Data.Sequence (Seq)
import Data.Text (Text)
import Looksee (Span)

type Loc = Span Int

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

data QualName = QualName
  { qnModule :: !(Seq Text)
  , qnVar :: !Text
  }
  deriving stock (Eq, Ord, Show)

data FileF r = File !(Seq Text) !(Seq r)
  deriving stock (Eq, Ord, Show)

type File = Anno Loc (FileF Decl)

data DeclF r
  = DeclSong !Text !Rational r
  | DeclPart !Text r
  deriving stock (Eq, Ord, Show)

type Decl = Anno Loc (DeclF Exp)

data ExpF r
  = ExpVar !QualName
  | ExpNote !Note
  | ExpApp !Fun !(Seq r)
  | ExpCheck !Beats r
  deriving stock (Eq, Ord, Show)

type Exp = Memo ExpF Loc

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
