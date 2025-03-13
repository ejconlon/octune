{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}

module Octune.Parser.Lexeme where

import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

lexeme :: Parser a -> Parser a
lexeme =
  L.lexeme $
    L.space
      space1
      (L.skipLineComment "--")
      (L.skipBlockComment "{-" "-}")

openSong :: Parser ()
openSong = void (lexeme (char '{'))

closeSong :: Parser ()
closeSong = void (lexeme (char '}'))

openSeq :: Parser ()
openSeq = void (lexeme (char '['))

closeSeq :: Parser ()
closeSeq = void (lexeme (char ']'))

openRepeat :: Parser ()
openRepeat = void (lexeme (string "[*"))

closeRepeat :: Parser ()
closeRepeat = void (lexeme (string "*]"))

openMerge :: Parser ()
openMerge = void (lexeme (string "[+"))

closeMerge :: Parser ()
closeMerge = void (lexeme (string "+]"))

openUsingWaveform :: Parser ()
openUsingWaveform = void (lexeme (string "[^"))

closeUsingWaveform :: Parser ()
closeUsingWaveform = void (lexeme (string "^]"))

openVolume :: Parser ()
openVolume = void (lexeme (string "[!"))

closeVolume :: Parser ()
closeVolume = void (lexeme (string "!]"))

openSubsection :: Parser ()
openSubsection = void (lexeme (string "[-"))

closeSubsection :: Parser ()
closeSubsection = void (lexeme (string "-]"))

moduleKW :: Parser Text
moduleKW = lexeme (string "module")

identifier :: Parser Text
identifier =
  T.pack
    <$> lexeme ((:) <$> lowerChar <*> many idChar <?> "identifier")
 where
  idChar :: Parser Char
  idChar = alphaNumChar <|> char '#'

integer :: Parser Int
integer = lexeme L.decimal

equal :: Parser ()
equal = void (lexeme (char '='))

colon :: Parser ()
colon = void (lexeme (char ':'))
