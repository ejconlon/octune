module Data.Topo where

import Control.Exception (Exception)
import Control.Monad.State.Strict (execState, modify', evalStateT, gets, evalState, StateT)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Sequence (Seq (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Typeable (Typeable)
import Control.Monad.Except (runExcept, throwError, runExceptT, Except)

data TopoErr k
  = TopoErrLoop !k
  | TopoErrMissing !k
  deriving stock (Eq, Ord, Show)

instance (Show k, Typeable k) => Exception (TopoErr k)

data SortSt k = SortSt
  { ssOutSeen :: !(Set k)
  , ssDeferSeen :: !(Set k)
  , ssOut :: !(Seq k)
  , ssDefer :: !(Seq (k, Set k))
  , ssWork :: ![k]
  }

topoSort ::(Ord k) => (v -> Set k) -> Map k v -> Either (TopoErr k) (Seq k)
topoSort findRef m = runExcept (evalStateT goNext initSt) where
  initSt = SortSt Set.empty Set.empty Empty Empty (Map.keys m)
  goNext = do
    work <- gets ssWork
    case work of
      [] -> do
        defer <- gets ssDefer
        case defer of
          Empty -> gets ssOut
          _ -> goUndefer
      k : ks -> do
        modify' (\ss -> ss { ssWork = ks })
        goDefer k
  goDefer k = do
    isOutSeen <- gets (Set.member k . ssOutSeen)
    if isOutSeen
      then goUndefer
      else do
        isDeferSeen <- gets (Set.member k . ssDeferSeen)
        if isDeferSeen
          then throwError (TopoErrLoop k)
          else case Map.lookup k m of
            Nothing -> throwError (TopoErrMissing k)
            Just v -> do
              modify' $ \ss ->
                let deps = Set.difference (findRef v) (ssOutSeen ss)
                    deferSeen' = Set.insert k (ssDeferSeen ss)
                    defer' = (k, deps) :<| ssDefer ss
                in ss { ssDeferSeen = deferSeen', ssDefer = defer' }
              goUndefer
  goUndefer = do
    defer <- gets ssDefer
    case defer of
      Empty -> goNext
      (k, dks) :<| defer' ->
        case Set.minView dks of
          Nothing -> do
            modify' $ \ss ->
              let outSeen' = Set.insert k (ssOutSeen ss)
                  out' = ssOut ss :|> k
              in ss { ssOutSeen = outSeen', ssOut = out', ssDefer = defer' }
            goUndefer
          Just (k', dks') -> do
            modify' $ \ss ->
              let defer'' = (k, dks') :<| defer'
              in ss { ssDefer = defer'' }
            goDefer k'

topoEvalEager :: (Ord k) => (v -> Set k) -> Map k v -> ((k -> w) -> v -> w) -> Either (TopoErr k) (Map k w)
topoEvalEager findRef m f = fmap (flip execState Map.empty . go) (topoSort findRef m)
 where
  go = \case
    Empty -> pure ()
    k :<| ks ->
      case Map.lookup k m of
        Nothing -> error "impossible"
        Just v -> do
          modify' (\c -> Map.insert k (f (c Map.!) v) c)
          go ks


-- data S = S !(Set k)
--
-- topoEvalLazy :: (Ord k) => (v -> Set k) -> Map k v -> ((k -> w) -> v -> w) -> k -> Either (TopoErr k) (Map k w)
-- topoEvalLazy findRef m f = fmap (flip execState= undefined
