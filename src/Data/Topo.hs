module Data.Topo
  ( SortErr (..)
  , topoSort
  , topoEval
  , topoAnnoM
  )
where

import Bowtie.Memo (Memo, memoKey, mkMemoM)
import Control.Exception (Exception)
import Control.Monad.Except (Except, runExcept, throwError)
import Control.Monad.State.Strict (StateT (..), execState, gets, modify')
import Data.Functor.Foldable (Base, Recursive (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Typeable (Typeable)

data SortErr k
  = SortErrLoop !k
  | SortErrMissing !k
  deriving stock (Eq, Ord, Show)

instance (Show k, Typeable k) => Exception (SortErr k)

data SortDeps k = SortDeps !k !(Set k)

data SortSt k = SortSt
  { ssOutSeen :: !(Set k)
  , ssDeferSeen :: !(Set k)
  , ssDefer :: !(Seq (SortDeps k))
  , ssWork :: !(Seq k)
  }

initSortSt :: [k] -> SortSt k
initSortSt = SortSt Set.empty Set.empty Empty . Seq.fromList

type SortM k = StateT (SortSt k) (Except (SortErr k))

runSortM :: SortM k a -> SortSt k -> Either (SortErr k) (a, SortSt k)
runSortM m = runExcept . runStateT m

topoSortNext :: (Ord k) => (k -> Maybe (Set k)) -> SortM k (Maybe k)
topoSortNext findRefs = goNext
 where
  goNext = do
    defer <- gets ssDefer
    case defer of
      Empty -> do
        work <- gets ssWork
        case work of
          Empty -> pure Nothing
          k :<| ks -> do
            modify' (\ss -> ss {ssWork = ks})
            goDefer k
      _ -> goUndefer
  goDefer k = do
    isOutSeen <- gets (Set.member k . ssOutSeen)
    if isOutSeen
      then goUndefer
      else do
        isDeferSeen <- gets (Set.member k . ssDeferSeen)
        if isDeferSeen
          then throwError (SortErrLoop k)
          else case findRefs k of
            Nothing -> throwError (SortErrMissing k)
            Just refs -> do
              modify' $ \ss ->
                let deps = Set.difference refs (ssOutSeen ss)
                    deferSeen' = Set.insert k (ssDeferSeen ss)
                    defer' = SortDeps k deps :<| ssDefer ss
                in  ss {ssDeferSeen = deferSeen', ssDefer = defer'}
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
              in  ss {ssOutSeen = outSeen', ssDeferSeen = deferSeen', ssDefer = defer'}
            pure (Just k)
          Just (k', dks') -> do
            modify' $ \ss ->
              let defer'' = SortDeps k dks' :<| defer'
              in  ss {ssDefer = defer''}
            goDefer k'

topoSort :: (Ord k) => (k -> Maybe (Set k)) -> [k] -> Either (SortErr k) (Seq k)
topoSort findRefs = fmap fst . runSortM (goGather Empty) . initSortSt
 where
  goGather !acc = do
    mk <- topoSortNext findRefs
    case mk of
      Nothing -> pure acc
      Just k -> goGather (acc :|> k)

topoEval :: (Ord k) => (v -> Set k) -> Map k v -> ((k -> w) -> v -> w) -> Either (SortErr k) (Map k w)
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

topoAnnoM
  :: (Monad m, Ord k, Recursive v, Base v ~ f, Traversable f)
  => (v -> Set k)
  -> Map k v
  -> ((k -> m w) -> f w -> m w)
  -> Either (SortErr k) (Map k (m (Memo f w)))
topoAnnoM findValRefs m f = fmap (flip execState Map.empty . go) (topoSort findRefs (Map.keys m))
 where
  findRefs = fmap findValRefs . flip Map.lookup m
  go = \case
    Empty -> pure ()
    k :<| ks ->
      case Map.lookup k m of
        Nothing -> error "impossible"
        Just v -> do
          modify' $ \c ->
            let g = f (fmap memoKey . (c Map.!))
                x = mkMemoM g v
            in  Map.insert k x c
          go ks
