{-# LANGUAGE OverloadedStrings #-}

module Octune.StaticAnalysis.VarUsage
  ( checkVarUsage
  )
where

import Control.Lens (to, (^.))
import Data.Either.Validation
  ( Validation (..)
  , validationToEither
  )
import Data.Foldable (Foldable (foldl', toList), traverse_)
import Data.Graph (Graph)
import Data.Graph qualified as Graph
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lens (IsText (packed))
import Octune.Types.AST (AST (..), QualifiedName (..))
import Octune.Types.Ann (Ann, pos)
import Octune.Types.Env (Env)
import Text.Megaparsec.Pos (sourcePosPretty)

-- TODO: Use a type for error throughout to express multiple errors
checkVarUsage :: Env (AST Ann) -> Either [Text] ()
checkVarUsage env =
  validationToEither $
    checkVarsDeclared env
      *> checkNoVarCycles env

-- Checks that variables used have all been declared
checkVarsDeclared :: Env (AST Ann) -> Validation [Text] ()
checkVarsDeclared env = traverse_ (uncurry checkDeclRhs) (Map.toList env)
 where
  checkDeclRhs :: QualifiedName -> AST Ann -> Validation [Text] ()
  checkDeclRhs qDeclName (Song _ _ expr) =
    checkDeclRhs qDeclName expr
  checkDeclRhs qDeclName (Var ann qVarName) =
    case Map.lookup qVarName env of
      Nothing ->
        Failure
          [ mconcat
              [ ann ^. pos . to sourcePosPretty . packed
              , ":\nUndefined variable `"
              , variableName qVarName
              , "` in module `"
              , T.intercalate "." (moduleQual qVarName)
              , "` used in the declaration of `"
              , variableName qDeclName
              , "` in module `"
              , T.intercalate "." (moduleQual qDeclName)
              , "`\n"
              ]
          ]
      Just _ ->
        pure ()
  checkDeclRhs _ LineNote {} =
    pure ()
  checkDeclRhs declName (LineApp _ _ args) =
    traverse_ (checkDeclRhs declName) args
  checkDeclRhs _ BeatsAssertion {} =
    pure ()
  checkDeclRhs _ _ = error "Should not have File or Decl from parsing"

-- Checks that usages of variables don't form a cycle
checkNoVarCycles :: Env (AST Ann) -> Validation [Text] ()
checkNoVarCycles env = errorOnSelfEdges *> errorOnCycles
 where
  edgesFromVar
    :: QualifiedName
    -> AST Ann
    -> (QualifiedName, QualifiedName, [QualifiedName])
  edgesFromVar v expr = (v, v, varsIn expr)

  varsIn :: AST Ann -> [QualifiedName]
  varsIn (Song _ _ expr) = varsIn expr
  varsIn (Var _ v) = [v]
  varsIn LineNote {} = []
  varsIn (LineApp _ _ args) = foldl' (\a c -> varsIn c ++ a) [] args
  varsIn BeatsAssertion {} = []
  varsIn _ = error "Should not have File or Decl from parsing"

  -- Graph from variables to the variables that appear in their declaration
  varGraph :: Graph
  (varGraph, varNodeFromVertex, _) =
    Graph.graphFromEdges $ fmap (uncurry edgesFromVar) (Map.toList env)

  varFromVertex :: Graph.Vertex -> QualifiedName
  varFromVertex vertex = let (v, _, _) = varNodeFromVertex vertex in v

  errorOnSelfEdges :: Validation [Text] ()
  errorOnSelfEdges =
    case filter (uncurry (==)) (Graph.edges varGraph) of
      [] -> pure ()
      cs ->
        Failure
          [ mconcat
              [ "Variables cannot reference themselves:\n"
              , T.unlines $ fmap ("    - " <>) badVars
              ]
          ]
       where
        showVar = denoteVar . varFromVertex . fst
        badVars = fmap showVar cs
  errorOnCycles :: Validation [Text] ()
  errorOnCycles =
    case filter ((> 1) . length) (Graph.scc varGraph) of
      [] -> pure ()
      cs ->
        Failure
          [ mconcat
              [ "Variable usages cannot form a cycle:\n"
              , T.unlines $ fmap ("    - " <>) badComponents
              ]
          ]
       where
        showComponent component =
          let cycleVars =
                fmap
                  (denoteVar . varFromVertex)
                  (toList component)
          in  case cycleVars of
                v : _ -> T.intercalate " -> " (cycleVars ++ [v, "..."])
                _ -> error "unhandled var case"
        badComponents = fmap showComponent cs

  denoteVar :: QualifiedName -> Text
  denoteVar qName =
    mconcat
      [ "`"
      , T.intercalate "." (moduleQual qName)
      , "."
      , variableName qName
      , "`"
      ]
