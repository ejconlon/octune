{-# LANGUAGE OverloadedStrings #-}

module Minipat.Octune.Parser where

import Data.Void (Void)
import Looksee qualified as L
import Looksee.Lexer qualified as LL
import Minipat.Octune.Ast

type P = L.Parser Void

waveformP :: P Waveform
waveformP =
  L.altP
    [ Square <$ L.textP_ "SQUARE"
    , Sawtooth <$ L.textP_ "SAWTOOTH"
    , Triangle <$ L.textP_ "TRIANGLE"
    ]

letterP :: P Letter
letterP =
  L.altP
    [ C <$ L.charP_ 'C'
    , D <$ L.charP_ 'D'
    , E <$ L.charP_ 'E'
    , F <$ L.charP_ 'F'
    , G <$ L.charP_ 'G'
    , A <$ L.charP_ 'A'
    , B <$ L.charP_ 'B'
    ]

accidentalP :: P Accidental
accidentalP =
  L.altP
    [ Flat <$ L.charP_ 'b'
    , Sharp <$ L.charP_ '#'
    ]

octaveP :: P Octave
octaveP = do
  i <- fmap fromInteger L.intP
  if i >= -1 && i <= 8
    then pure i
    else fail "octave out of range [-1, 8]"

percussionP :: P Percussion
percussionP = do
  L.charP_ '%'
  mu <- L.optP (L.charP_ '%')
  pure (maybe Snare (const Clap) mu)

soundP :: P Sound
soundP =
  L.altP
    [ Rest <$ L.charP_ '_'
    , Drum <$> percussionP
    , Pitch <$> letterP <*> L.optP accidentalP <*> octaveP
    ]

noteModifierP :: P NoteModifier
noteModifierP = do
  _ <- L.charP_ '\''
  mu <- L.optP (L.charP_ '\'')
  pure (maybe Detached (const Staccato) mu)

beatsP :: P Beats
beatsP = L.altP [relP, L.udecP]
 where
  baseP =
    L.altP
      [ 2 <$ L.charP_ 'h'
      , 1 <$ L.charP_ 'q'
      , 0.5 <$ L.charP_ 'e'
      , 0.25 <$ L.charP_ 's'
      , 0.125 <$ L.charP_ 't'
      ]

  relP = do
    base <- baseP
    dots <- L.dropWhileP (== '.')
    pure (base * (2 - (1 / 2) ^^ dots))

lexP :: P a -> P a
lexP =
  LL.lexeme $
    LL.space
      L.space1P
      (LL.skipLineComment "--")
      (LL.skipBlockComment "{-" "-}")

noteP :: P Note
noteP = lexP (Note <$> L.repeatP noteModifierP <*> beatsP <*> soundP)
