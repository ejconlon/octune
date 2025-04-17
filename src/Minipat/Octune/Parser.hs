{-# LANGUAGE OverloadedStrings #-}

module Minipat.Octune.Parser where

import Bowtie.Anno (Anno (..))
import Bowtie.Memo (Memo, pattern MemoP)
import Control.Applicative ((<|>))
import Control.Exception (throwIO)
import Data.Char (isAlphaNum, isLower, isUpper)
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
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

satP :: String -> (Char -> Bool) -> P Char
satP msg p = do
  c <- L.headP
  if p c
    then pure c
    else fail msg

upperCharP :: P Char
upperCharP = satP "not upper case" isUpper

lowerCharP :: P Char
lowerCharP = satP "not lower case" isLower

lowerTextP :: P Text
lowerTextP = L.takeWhileP isLower

identTrailP :: P Text
identTrailP = L.takeWhileP (\c -> isAlphaNum c || c == '#')

moduleComponentP :: P Text
moduleComponentP = T.cons <$> upperCharP <*> lowerTextP

identP :: P Text
identP = T.cons <$> lowerCharP <*> identTrailP

moduleSymP :: P ()
moduleSymP = lexP (L.textP_ "module")

moduleNameP :: P (Seq Text)
moduleNameP = L.sepByP (L.charP_ '.') moduleComponentP

moduleDeclP :: P (Seq Text)
moduleDeclP = lexP (moduleSymP *> moduleNameP)

qualNameP :: P QualName
qualNameP = lexP $ do
  m <- fmap (fromMaybe Empty) $ L.optP $ do
    m <- moduleNameP
    L.charP_ '.'
    pure m
  fmap (QualName m) identP

annoP :: P a -> P (Anno Loc a)
annoP = L.spanAroundP Anno

memoP :: P (f (Memo f Loc)) -> P (Memo f Loc)
memoP = L.spanAroundP MemoP

openSongP
  , closeSongP
  , openSequP
  , closeSequP
  , openRepeatP
  , closeRepeatP
  , openMergeP
  , closeMergeP
  , openUsingWfP
  , closeUsingWfP
  , openVolumeP
  , closeVolumeP
  , openSliceP
  , closeSliceP
  , openCheckP
  , closeCheckP
  , colonP
  , equalsP
  , tildeP
    :: P ()
openSongP = lexP (L.charP_ '{')
closeSongP = lexP (L.charP_ '}')
openSequP = lexP (L.charP_ '[')
closeSequP = lexP (L.charP_ ']')
openRepeatP = lexP (L.textP_ "[*")
closeRepeatP = lexP (L.textP_ "*]")
openMergeP = lexP (L.textP_ "[+")
closeMergeP = lexP (L.textP_ "+]")
openUsingWfP = lexP (L.textP_ "[^")
closeUsingWfP = lexP (L.textP_ "^]")
openVolumeP = lexP (L.textP_ "[!")
closeVolumeP = lexP (L.textP_ "!]")
openSliceP = lexP (L.textP_ "[-")
closeSliceP = lexP (L.textP_ "-]")
openCheckP = lexP (L.textP_ "[#")
closeCheckP = lexP (L.textP_ "#]")
colonP = lexP (L.charP_ ':')
equalsP = lexP (L.charP_ '=')
tildeP = lexP (L.charP_ '=')

fileP :: P File
fileP = lexP (annoP (File <$> moduleDeclP <*> L.repeatP declP))

declP :: P Decl
declP = songP <|> partP

songP :: P Decl
songP = lexP $ annoP $ do
  ident <- lexP identP
  equalsP
  L.betweenP openSongP closeSongP $ do
    bpm <- fmap fromInteger (lexP L.uintP)
    colonP
    fmap (DeclSong ident bpm) expP

partP :: P Decl
partP = lexP $ annoP $ do
  ident <- lexP identP
  equalsP
  fmap (DeclPart ident) expP

expP :: P Exp
expP = L.altP [noteExpP, checkExpP, appExpP, varExpP]

noteExpP :: P Exp
noteExpP = lexP (memoP (ExpNote <$> noteP))

checkExpP :: P Exp
checkExpP = lexP $ memoP $ L.betweenP openCheckP closeCheckP $ do
  beats <- lexP L.udecP
  colonP
  fmap (ExpCheck beats) expP

varExpP :: P Exp
varExpP = lexP (memoP (ExpVar <$> qualNameP))

appExpP :: P Exp
appExpP =
  L.altP
    [ appRepeatP
    , appMergeP
    , appChordP
    , appUsingWfP
    , appVolumeP
    , appSliceP
    , appSequP
    ]

appRepeatP :: P Exp
appRepeatP = lexP $ memoP $ L.betweenP openRepeatP closeRepeatP $ do
  times <- lexP L.udecP
  colonP
  fmap (ExpApp (FunRepeat times)) (L.repeatP expP)

appMergeP :: P Exp
appMergeP = lexP $ memoP $ L.betweenP openMergeP closeMergeP $ do
  fmap (ExpApp FunMerge) (L.repeatP expP)

appChordP :: P Exp
appChordP = lexP $ memoP $ L.betweenP openMergeP closeMergeP $ do
  nodeMods <- L.repeatP noteModifierP
  beats <- lexP L.udecP
  colonP
  pitches <- L.repeatP (lexP (annoP soundP))
  let notes = flip fmap pitches $ \(Anno loc n) ->
        MemoP loc (ExpNote (Note nodeMods beats n))
  pure (ExpApp FunMerge notes)

appUsingWfP :: P Exp
appUsingWfP = lexP $ memoP $ L.betweenP openUsingWfP closeUsingWfP $ do
  wf <- lexP waveformP
  colonP
  fmap (ExpApp (FunWaveform wf)) (L.repeatP expP)

appVolumeP :: P Exp
appVolumeP = lexP $ memoP $ L.betweenP openVolumeP closeVolumeP $ do
  vol <- lexP L.udecP
  colonP
  fmap (ExpApp (FunVolume vol)) (L.repeatP expP)

appSliceP :: P Exp
appSliceP = lexP $ memoP $ L.betweenP openSliceP closeSliceP $ do
  start <- lexP L.udecP
  tildeP
  end <- lexP L.udecP
  colonP
  fmap (ExpApp (FunSlice start end)) (L.repeatP expP)

appSequP :: P Exp
appSequP = lexP $ memoP $ L.betweenP openSequP closeSequP $ do
  fmap (ExpApp FunSequ) (L.repeatP expP)

parseI :: P a -> String -> IO a
parseI p s = L.parseI p (T.pack s) >>= either throwIO pure

parseF :: FilePath -> IO File
parseF fp = do
  contents <- TIO.readFile fp
  either throwIO pure (L.parse fileP contents)
