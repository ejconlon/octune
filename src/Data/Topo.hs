module Data.Topo
  ( TopoErr (..)
  , topoSort
  , topoEval
  ) where

import Control.Exception (Exception)
import Control.Monad.State.Strict (execState, modify', gets, StateT (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Sequence (Seq (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Typeable (Typeable)
import Control.Monad.Except (runExcept, throwError, Except)
import qualified Data.Sequence as Seq

data TopoErr k
  = TopoErrLoop !k
  | TopoErrMissing !k
  deriving stock (Eq, Ord, Show)

instance (Show k, Typeable k) => Exception (TopoErr k)

data SortDeps k = SortDeps !k !(Set k)

data SortSt k = SortSt
  { ssOutSeen :: !(Set k)
  , ssDeferSeen :: !(Set k)
  , ssDefer :: !(Seq (SortDeps k))
  , ssWork :: !(Seq k)
  }

initSortSt :: [k] -> SortSt k
initSortSt = SortSt Set.empty Set.empty Empty . Seq.fromList

type SortM k = StateT (SortSt k) (Except (TopoErr k))

runSortM :: SortM k a -> SortSt k -> Either (TopoErr k) (a, SortSt k)
runSortM m = runExcept . runStateT m

topoSortNext :: (Ord k) => (k -> Maybe (Set k)) -> SortM k (Maybe k)
topoSortNext findRefs = goNext where
  goNext = do
    work <- gets ssWork
    case work of
      Empty -> do
        defer <- gets ssDefer
        case defer of
          Empty -> pure Nothing
          _ -> goUndefer
      k :<| ks -> do
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
          else case findRefs k of
            Nothing -> throwError (TopoErrMissing k)
            Just refs -> do
              modify' $ \ss ->
                let deps = Set.difference refs (ssOutSeen ss)
                    deferSeen' = Set.insert k (ssDeferSeen ss)
                    defer' = SortDeps k deps :<| ssDefer ss
                in ss { ssDeferSeen = deferSeen', ssDefer = defer' }
              goUndefer
  goUndefer = do
    defer <- gets ssDefer
    case defer of
      Empty -> goNext
      SortDeps k dks :<| defer' ->
        case Set.minView dks of
          Nothing -> do
            modify' $ \ss ->
              let outSeen' = Set.insert k (ssOutSeen ss)
                  deferSeen' = Set.delete k (ssDeferSeen ss)
              in ss { ssOutSeen = outSeen', ssDeferSeen = deferSeen', ssDefer = defer' }
            pure (Just k)
          Just (k', dks') -> do
            modify' $ \ss ->
              let defer'' = SortDeps k dks' :<| defer'
              in ss { ssDefer = defer'' }
            goDefer k'

topoSortAdd :: (Ord k) => [k] -> SortM k ()
topoSortAdd ks = modify' $ \ss ->
  let ks' = Seq.fromList (filter (\k -> not (Set.member k (ssOutSeen ss) || Set.member k (ssDeferSeen ss))) ks)
  in ss { ssWork = ssWork ss <> ks' }

topoSort :: (Ord k) => (k -> Maybe (Set k)) -> [k] -> Either (TopoErr k) (Seq k)
topoSort findRefs = fmap fst . runSortM (goGather Empty) . initSortSt where
  goGather !acc = do
    mk <- topoSortNext findRefs
    case mk of
      Nothing -> pure acc
      Just k -> goGather (acc :|> k)

topoEval :: (Ord k) => (v -> Set k) -> Map k v -> ((k -> w) -> v -> w) -> Either (TopoErr k) (Map k w)
topoEval findValRefs m f = fmap (flip execState Map.empty . go) (topoSort findRefs (Map.keys m))
 where
  findRefs = fmap findValRefs . flip Map.lookup m
  go = \case
    Empty -> pure ()
    k :<| ks ->
      case Map.lookup k m of
        Nothing -> error "impossible"
        Just v -> do
          modify' (\c -> Map.insert k (f (c Map.!) v) c)
          go ks

