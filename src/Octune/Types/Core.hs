module Octune.Types.Core where

import Octune.Types.AST (AST (..), LineFun, QualifiedName)
import Octune.Types.Env (Env)
import Octune.Types.Note (Note)

-- Resulting from after static analysis
data Core
  = CoreSong !Int Core
  | CoreVar !QualifiedName
  | CoreNote !Note
  | CoreApp !LineFun ![Core]
  deriving (Show, Read, Eq)

coreEnv :: Env (AST a) -> Env Core
coreEnv = fmap go
 where
  go :: AST a -> Core
  go (Song _ bpm lExpr) = CoreSong bpm (go lExpr)
  go (Var _ qName) = CoreVar qName
  go (LineNote _ note) = CoreNote note
  go (LineApp _ lFun lExprs) = CoreApp lFun (go <$> filter keep lExprs)
  go _ = error "Arg should only have song or line expressions"

  keep BeatsAssertion {} = False
  keep _ = True
