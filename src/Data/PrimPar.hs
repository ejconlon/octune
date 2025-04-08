module Data.PrimPar
  ( ParMonad
  , Mutex
  , newMutex
  , withMutex
  , modifyMutex
  , modifyMutex_
  , readMutex
  , PrimPar
  , runPrimPar
  , ReadPar
  , runReadPar
  , ParEvalErr (..)
  , parEval
  , parEvalInc
  , parFor_
  )
where

import Control.DeepSeq (NFData (..))
import Control.Exception (Exception)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Par (InclusiveRange (..), parFor)
import Control.Monad.Par.Class (ParFuture (..), ParIVar (..))
import Control.Monad.Par.IO (ParIO)
import Control.Monad.Par.IO qualified as PIO
import Control.Monad.Par.Scheds.TraceInternal (IVar)
import Control.Monad.Primitive (PrimMonad (..), RealWorld)
import Control.Monad.Reader (MonadReader, ReaderT (..))
import Data.Foldable (for_, toList)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import Data.Primitive.MVar (MVar, newMVar, putMVar, readMVar, takeMVar)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Traversable (for)
import Data.Typeable (Typeable)
import GHC.IO (IO (..))

type ParMonad m = (PrimMonad m, PrimState m ~ RealWorld, ParFuture IVar m, ParIVar IVar m)

newtype Mutex a = Mutex {unMutex :: MVar RealWorld a}
  deriving stock (Eq)

newMutex :: (ParMonad m) => a -> m (Mutex a)
newMutex = fmap Mutex . newMVar

withMutex :: (ParMonad m) => Mutex a -> (a -> m b) -> m b
withMutex (Mutex var) f = do
  a <- takeMVar var
  b <- f a
  putMVar var a
  pure b

modifyMutex :: (ParMonad m) => Mutex a -> (a -> m (a, b)) -> m b
modifyMutex (Mutex var) f = do
  a <- takeMVar var
  (a', b) <- f a
  putMVar var a'
  pure b

modifyMutex_ :: (ParMonad m) => Mutex a -> (a -> m a) -> m ()
modifyMutex_ (Mutex var) f = do
  a <- takeMVar var
  a' <- f a
  putMVar var a'

readMutex :: (ParMonad m) => Mutex a -> m a
readMutex (Mutex var) = readMVar var

newtype PrimPar a = PrimPar {unPrimPar :: ParIO a}
  deriving newtype (Functor, Applicative, Monad, ParFuture IVar, ParIVar IVar)

runPrimPar :: PrimPar a -> IO a
runPrimPar = PIO.runParIO . unPrimPar

instance PrimMonad PrimPar where
  type PrimState PrimPar = RealWorld
  primitive f = PrimPar (liftIO (IO f))

newtype ReadPar r a = ReadPar {unReadPar :: ReaderT r PrimPar a}
  deriving newtype (Functor, Applicative, Monad, MonadReader r)

runReadPar :: ReadPar r a -> r -> IO a
runReadPar m = runPrimPar . runReaderT (unReadPar m)

instance ParFuture IVar (ReadPar r) where
  spawn_ m = ReadPar (ReaderT (spawn_ . runReaderT (unReadPar m)))
  get = ReadPar . ReaderT . const . get

instance ParIVar IVar (ReadPar r) where
  new = ReadPar (ReaderT (const new))
  fork m = ReadPar (ReaderT (fork . runReaderT (unReadPar m)))
  put_ i = ReadPar . ReaderT . const . put_ i

instance PrimMonad (ReadPar r) where
  type PrimState (ReadPar r) = RealWorld
  primitive f = ReadPar $ ReaderT $ const $ primitive f

newtype ParEvalErr k = ParEvalErr k
  deriving stock (Eq, Ord, Show)

instance (Show k, Typeable k) => Exception (ParEvalErr k)

data Env k v = Env
  { envOk :: !Bool
  , envVars :: !(Map k (IVar (Maybe v)))
  }

parEval
  :: (ParMonad m, Ord k, NFData v)
  => (k -> Maybe x)
  -> (x -> Set k)
  -> ((k -> m v) -> x -> m v)
  -> k
  -> m (Either (ParEvalErr k) v)
parEval getDef getDeps mkVal root = fmap (fmap fst) (parEvalInc getDef getDeps mkVal root Map.empty)

parEvalInc
  :: (ParMonad m, Ord k, NFData v)
  => (k -> Maybe x)
  -> (x -> Set k)
  -> ((k -> m v) -> x -> m v)
  -> k
  -> Map k v
  -> m (Either (ParEvalErr k) (v, Map k v))
parEvalInc getDef getDeps mkVal root startVals = goTop
 where
  goTop = do
    errVar <- new
    startVars <- traverse (newFull . Just) startVals
    envVar <- newMutex (Env True startVars)
    rootVal <- goFork errVar envVar root >>= get
    env <- readMutex envVar
    if envOk env
      then do
        pairs <- traverse (\(k, i) -> fmap ((k,) . fromJust) (get i)) (Map.toList (envVars env))
        pure (Right (fromJust rootVal, Map.fromList pairs))
      else fmap Left (get errVar)
  goFork errVar envVar k = do
    resVar <- new
    (shouldFork, resVar') <- modifyMutex envVar $ \env ->
      pure $
        if envOk env
          then case Map.lookup k (envVars env) of
            Nothing -> (env {envVars = Map.insert k resVar (envVars env)}, (True, resVar))
            Just resVar' -> (env, (False, resVar'))
          else (env, (False, resVar))
    if shouldFork
      then fork (goSub errVar envVar k)
      else put resVar Nothing
    pure resVar'
  goSub errVar envVar k = do
    res <- case getDef k of
      Nothing -> do
        shouldPut <- modifyMutex envVar $ \env ->
          pure (env {envOk = False}, envOk env)
        when shouldPut (put_ errVar (ParEvalErr k))
        pure Nothing
      Just x -> do
        let ks = getDeps x
        vars <- for (toList ks) (goFork errVar envVar)
        for_ vars get
        shouldMk <- withMutex envVar (pure . envOk)
        if shouldMk
          then fmap Just (goMk envVar x)
          else pure Nothing
    resVar <- withMutex envVar (pure . (Map.! k) . envVars)
    put resVar res
  goMk envVar = mkVal (\k -> fmap fromJust (get =<< withMutex envVar (pure . (Map.! k) . envVars)))

parFor_
  :: (ParMonad m, Traversable t, NFData v)
  => t k
  -> (k -> m v)
  -> m ()
parFor_ tk f = do
  ivs <- traverse (spawn . f) tk
  for_ ivs get
