module Minipat.Octune.Topo
  ( SortErr (..)
  , topoSort
  , topoSortInc
  , topoEval
  , topoEvalInc
  , topoAnno
  , topoAnnoInc
  , topoAnnoM
  , topoAnnoIncM
  )
where

import Bowtie.Memo (Memo (..), MemoF (..), memoFix, memoKey, reMkMemoM)
import Control.Exception (Exception)
import Control.Monad.Except (Except, runExcept, throwError)
import Control.Monad.Identity (Identity (..))
import Control.Monad.State.Strict (StateT (..), execState, execStateT, gets, modify')
import Control.Monad.Trans (lift)
import Data.Foldable (foldl')
import Data.Functor.Foldable (Recursive (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
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

initSortSt :: (Ord k) => Set k -> [k] -> SortSt k
initSortSt out work = SortSt (foldl' (flip Set.delete) out work) Set.empty Empty (Seq.fromList work)

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
topoSort = topoSortInc Set.empty

topoSortInc :: (Ord k) => Set k -> (k -> Maybe (Set k)) -> [k] -> Either (SortErr k) (Seq k)
topoSortInc st0 findRefs = fmap fst . runSortM (goGather Empty) . initSortSt st0
 where
  goGather !acc = do
    mk <- topoSortNext findRefs
    case mk of
      Nothing -> pure acc
      Just k -> goGather (acc :|> k)

topoEval :: (Ord k) => (v -> Set k) -> Map k v -> ((k -> w) -> k -> v -> w) -> Either (SortErr k) (Map k w)
topoEval = topoEvalInc Map.empty

topoEvalInc
  :: (Ord k) => Map k w -> (v -> Set k) -> Map k v -> ((k -> w) -> k -> v -> w) -> Either (SortErr k) (Map k w)
topoEvalInc n findValRefs m f = fmap (flip execState n . go) (topoSortInc (Map.keysSet n) findRefs (Map.keys m))
 where
  findRefs = fmap findValRefs . flip Map.lookup m
  go = \case
    Empty -> pure ()
    k :<| ks ->
      case Map.lookup k m of
        Nothing -> error "invalid key fn"
        Just v -> do
          modify' $ \c ->
            let r j = fromMaybe (c Map.! j) (Map.lookup j n)
            in  Map.insert k (f r k v) c
          go ks

topoAnno
  :: (Ord k, Traversable f)
  => (f (Set k) -> Set k)
  -> Map k (Memo f z)
  -> ((k -> w) -> k -> z -> f w -> w)
  -> Either (SortErr k) (Map k (Memo f w))
topoAnno = topoAnnoInc Map.empty

topoAnnoInc
  :: (Ord k, Traversable f)
  => Map k w
  -> (f (Set k) -> Set k)
  -> Map k (Memo f z)
  -> ((k -> w) -> k -> z -> f w -> w)
  -> Either (SortErr k) (Map k (Memo f w))
topoAnnoInc n findValRefs m f = fmap runIdentity (topoAnnoIncM n findValRefs m (\onK k z -> Identity . f (runIdentity . onK) k z))

topoAnnoM
  :: (Monad m, Ord k, Traversable f)
  => (f (Set k) -> Set k)
  -> Map k (Memo f z)
  -> ((k -> m w) -> k -> z -> f w -> m w)
  -> Either (SortErr k) (m (Map k (Memo f w)))
topoAnnoM = topoAnnoIncM Map.empty

topoAnnoIncM
  :: (Monad m, Ord k, Traversable f)
  => Map k w
  -> (f (Set k) -> Set k)
  -> Map k (Memo f z)
  -> ((k -> m w) -> k -> z -> f w -> m w)
  -> Either (SortErr k) (m (Map k (Memo f w)))
topoAnnoIncM n findValRefs m f = fmap (flip execStateT Map.empty . go) (topoSortInc (Map.keysSet n) findRefs (Map.keys m))
 where
  findRefs = fmap (cata findValRefs . memoFix) . flip Map.lookup m
  go = \case
    Empty -> pure ()
    k :<| ks ->
      case Map.lookup k m of
        Nothing -> error "invalid key fn"
        Just v -> do
          g <- gets $ \c ->
            let r j = pure (fromMaybe (memoKey (c Map.! j)) (Map.lookup j n))
            in  \(MemoFP z fow) -> f r k z (fmap memoKey fow)
          x <- lift (reMkMemoM g v)
          modify' (Map.insert k x)
          go ks
