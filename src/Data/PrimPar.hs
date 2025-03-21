module Data.PrimPar
  ( PrimPar
  , runPrimPar
  , ReadPar
  , runReadPar
  )
where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Par ()
import Control.Monad.Par.Class (ParFuture (..))
import Control.Monad.Par.IO (ParIO)
import Control.Monad.Par.IO qualified as PIO
import Control.Monad.Par.Scheds.TraceInternal (IVar)
import Control.Monad.Primitive (PrimMonad (..), RealWorld)
import Control.Monad.Reader (MonadReader, ReaderT (..))
import GHC.IO (IO (..))

newtype PrimPar a = PrimPar {unPrimPar :: ParIO a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, ParFuture IVar)

runPrimPar :: PrimPar a -> IO a
runPrimPar = PIO.runParIO . unPrimPar

instance PrimMonad PrimPar where
  type PrimState PrimPar = RealWorld
  primitive f = PrimPar (liftIO (IO f))

instance MonadFail PrimPar where
  fail = liftIO . fail

newtype ReadPar r a = ReadPar {unReadPar :: ReaderT r PrimPar a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadFail, MonadReader r)

runReadPar :: ReadPar r a -> r -> IO a
runReadPar m = runPrimPar . runReaderT (unReadPar m)

instance ParFuture IVar (ReadPar r) where
  -- spawn_ :: ReadPar r a -> ReadPar r (IVar a)
  spawn_ m = ReadPar (ReaderT (spawn_ . runReaderT (unReadPar m)))

  -- get :: IVar a -> ReadPar r a
  get = ReadPar . ReaderT . const . get

instance PrimMonad (ReadPar r) where
  type PrimState (ReadPar r) = RealWorld
  primitive f = ReadPar (ReaderT (const (liftIO (IO f))))
