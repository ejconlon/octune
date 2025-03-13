{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}

module Octune.Parser.AST where

import Data.Combinator ((<^>))
import Data.Text (Text)
import qualified Data.Text as T
import Octune.Parser.Lexeme
  ( Parser
  , closeMerge
  , closeRepeat
  , closeSeq
  , closeSong
  , closeSubsection
  , closeUsingWaveform
  , closeVolume
  , colon
  , equal
  , identifier
  , integer
  , lexeme
  , moduleKW
  , openMerge
  , openRepeat
  , openSeq
  , openSong
  , openSubsection
  , openUsingWaveform
  , openVolume
  )
import Octune.Parser.Note
  ( pBeats
  , pNote
  , pNoteModifier
  , pRational
  , pSound
  )
import Octune.Types.AST
  ( AST (..)
  , LineFun (..)
  , QualifiedName (..)
  , Waveform (..)
  )
import Octune.Types.Ann (Ann (..))
import Octune.Types.Note (Note (..))
import Text.Megaparsec
  ( MonadParsec (..)
  , SourcePos
  , between
  , getSourcePos
  , many
  , optional
  , sepBy1
  , some
  , (<|>)
  )
import Text.Megaparsec.Char
  ( char
  , letterChar
  , space
  , string
  , upperChar
  )

initAnn :: SourcePos -> Ann
initAnn srcPos =
  Ann
    { _pos = srcPos
    , _beatLength = Nothing
    }

getAnn :: Parser Ann
getAnn = initAnn <$> getSourcePos

pWaveform :: Parser Waveform
pWaveform =
  lexeme $
    Square
      <$ string "SQUARE"
        <|> Sawtooth
      <$ string "SAWTOOTH"
        <|> Triangle
      <$ string "TRIANGLE"

pModuleComponent :: Parser Text
pModuleComponent = T.pack <$> ((:) <$> upperChar <*> many letterChar)

pModuleDeclaration :: Parser [Text]
pModuleDeclaration =
  lexeme $
    moduleKW *> pModuleComponent `sepBy1` char '.'

pFile :: Parser (AST Ann)
pFile = lexeme space *> pFileBase
 where
  pFileBase =
    File
      <$> getAnn
      <*> pModuleDeclaration
      <*> some pDecl
      <* eof

pDecl :: Parser (AST Ann)
pDecl = Decl <$> getAnn <*> (identifier <* equal) <*> pRhs
 where
  pRhs = try pLineExpr <|> pSongExpr

pSongExpr :: Parser (AST Ann)
pSongExpr =
  between openSong closeSong $
    Song <$> getAnn <*> integer <* colon <*> pLineExpr

pLineExpr :: Parser (AST Ann)
pLineExpr = try pLineNote <|> pVar <|> pLineApp

pQualifiedName :: Parser QualifiedName
pQualifiedName =
  QualName <$> many (pModuleComponent <* char '.') <*> identifier

pVar :: Parser (AST Ann)
pVar = Var <$> getAnn <*> pQualifiedName

pLineNote :: Parser (AST Ann)
pLineNote = LineNote <$> getAnn <*> pNote

pLineApp :: Parser (AST Ann)
pLineApp =
  pRepeatApp
    <|> try pMergeApp
    <|> pChord
    <|> pUsingWaveformApp
    <|> pVolumeApp
    <|> pSubsectionApp
    <|> pSeqApp
 where
  pRepeatApp =
    between openRepeat closeRepeat $
      LineApp
        <$> getAnn
        <*> (Repeat <$> (integer <* colon))
        <*> some (pBeatAssert <|> pLineExpr)
  pMergeApp =
    between openMerge closeMerge $
      LineApp
        <$> getAnn
          <^> Merge
        <*> some pLineExpr
  pUsingWaveformApp =
    between openUsingWaveform closeUsingWaveform $
      LineApp
        <$> getAnn
        <*> (UsingWaveform <$> (pWaveform <* colon))
        <*> some (pBeatAssert <|> pLineExpr)
  pVolumeApp =
    between openVolume closeVolume $
      LineApp
        <$> getAnn
        <*> (Volume <$> (lexeme pRational <* colon))
        <*> some (pBeatAssert <|> pLineExpr)
  pSubsectionApp =
    between openSubsection closeSubsection $
      LineApp
        <$> getAnn
        <*> ( Subsection
                <$> (pRational <* char '~')
                <*> (lexeme pRational <* colon)
            )
        <*> some (pBeatAssert <|> pLineExpr)
  pSeqApp =
    between openSeq closeSeq $
      LineApp
        <$> getAnn
          <^> Seq
        <*> some (pBeatAssert <|> pLineExpr)

pBeatAssert :: Parser (AST Ann)
pBeatAssert =
  lexeme $
    BeatsAssertion
      <$> getAnn
      <*> (char '|' *> optional (pBeats <* char '>'))

-- Syntactic sugar

-- Parses `[+ mb : p1 ... pn +]`, -- equivalent to `[+ mbp1 ... mbpn +]`
-- where `m` is a note modifier list, `b` is a beat count and
-- `p1`, ..., `pn` are pitches
pChord :: Parser (AST Ann)
pChord = between openMerge closeMerge $ do
  ann <- getAnn
  noteMods <- many pNoteModifier
  beats <- lexeme pBeats
  colon
  pitches <- many (lexeme pSound)
  let notes = Note noteMods beats <$> pitches
  pure $ LineApp ann Merge (LineNote ann <$> notes)
