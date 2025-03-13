{-# LANGUAGE OverloadedStrings #-}

module Octune.StaticAnalysis.BarBeats where

import Control.Lens (sumOf, to, traversed, (^.), _Just)
import Control.Monad (when)
import Data.Either.Validation
  ( Validation (Failure)
  , validationToEither
  )
import Data.Foldable (traverse_)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Real (Ratio (..))
import Octune.Types.AST (AST (..), LineFun (..), annotation)
import Octune.Types.Ann (Ann, beatLength, pos)
import Octune.Types.Env (Env)
import Text.Megaparsec.Pos (sourcePosPretty)

checkBeatsAssertions :: Env (AST Ann) -> Either [Text] ()
checkBeatsAssertions = validationToEither . traverse_ go
 where
  go :: AST Ann -> Validation [Text] ()
  go (Song _ _ expr) = go expr
  go Var {} = pure ()
  go LineNote {} = pure ()
  go BeatsAssertion {} = pure ()
  go (LineApp _ lFun args) =
    when (hasBeatsAssertionsArgs lFun) (checkBeatsList args)
      *> traverse_ go args
  go _ = error "Should only have Song and Line expressions in Env"

  hasBeatsAssertionsArgs :: LineFun -> Bool
  hasBeatsAssertionsArgs Merge = False
  hasBeatsAssertionsArgs _ = True

  -- TODO: figure out how to build a traversal focusing on "bars"
  --       paired with their expected beats
  checkBeatsList :: [AST Ann] -> Validation [Text] ()
  checkBeatsList [] = pure ()
  checkBeatsList (BeatsAssertion ann mBeats : es) =
    let (curBar, nextBar) = span notBeatsAssertion es
        curBarLength =
          sumOf (traversed . annotation . beatLength . _Just) curBar
    in  case mBeats of
          Nothing -> checkBeatsList nextBar
          Just beats
            | beats == curBarLength -> checkBeatsList nextBar
            | otherwise ->
                Failure
                  [ T.pack $
                      mconcat
                        [ ann ^. pos . to sourcePosPretty
                        , ":"
                        , "\n    - Beat assertion failure"
                        , "\n      Expected beats: "
                        , showRational beats
                        , "\n        Actual beats: "
                        , showRational curBarLength
                        ]
                  ]
                  *> checkBeatsList nextBar
  checkBeatsList (_ : es) = checkBeatsList es

  notBeatsAssertion :: AST Ann -> Bool
  notBeatsAssertion BeatsAssertion {} = False
  notBeatsAssertion _ = True

  showRational :: Rational -> String
  showRational (n :% 1) = show n
  showRational (n :% d) = show n ++ "/" ++ show d
