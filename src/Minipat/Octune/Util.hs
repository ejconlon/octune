module Minipat.Octune.Util where

import Bowtie (Anno (..), Memo, memoKey, pattern MemoP)
import Control.Monad (when)
import Control.Monad.Primitive (PrimMonad (..))
import Control.Monad.Reader (ReaderT (..))
import Data.Foldable (for_)
import Data.Primitive (Prim)
import Data.Primitive.PrimArray
  ( MutablePrimArray
  , PrimArray
  , copyPrimArray
  , emptyPrimArray
  , indexPrimArray
  , newPrimArray
  , readPrimArray
  , runPrimArray
  , setPrimArray
  , sizeofPrimArray
  , writePrimArray
  )
import Data.STRef.Strict (newSTRef, readSTRef, writeSTRef)
import Data.Semigroup (Max (..), Sum (..))
import Debug.Trace (trace)

-- Debug utils

-- | A utility function for debugging that traces a list of strings.
traceAll :: [[String]] -> a -> a
traceAll xs = if False then id else trace ("\n<==\n" ++ unlines (fmap (("* " ++) . unwords) xs) ++ "==>\n")

-- Bowtie utils

-- | A utility function for recalling memoized values in a monadic context.
memoRecallM :: (Monad m, Traversable f) => (f (Anno k v) -> ReaderT k m v) -> Memo f k -> m v
memoRecallM f = go
 where
  go (MemoP k fm) = traverse (\m -> fmap (Anno (memoKey m)) (go m)) fm >>= \fa -> runReaderT (f fa) k

-- Prim adapters

-- | Create a new primitive array filled with zeros.
zeroPrimArray :: (PrimMonad m, Prim a, Num a) => Int -> m (MutablePrimArray (PrimState m) a)
zeroPrimArray n = do
  marr <- newPrimArray n
  setPrimArray marr 0 n 0
  pure marr

-- | Replicate a primitive array a given number of times.
replicateWholePrimArray :: (Prim a) => Int -> PrimArray a -> PrimArray a
replicateWholePrimArray n sarr =
  if
    | n <= 0 -> emptyPrimArray
    | n == 1 -> sarr
    | otherwise -> runPrimArray $ do
        let srcSize = sizeofPrimArray sarr
            len = n * srcSize
        darr <- newPrimArray len
        for_ [0 .. n - 1] $ \i -> do
          let pos = i * srcSize
          copyPrimArray darr pos sarr 0 srcSize
        pure darr

-- | Concatenate multiple primitive arrays into one.
concatPrimArray :: (Prim a) => [PrimArray a] -> PrimArray a
concatPrimArray = \case
  [] -> emptyPrimArray
  [s0] -> s0
  ss -> runPrimArray $ do
    let totLen = getSum (foldMap (Sum . sizeofPrimArray) ss)
    darr <- newPrimArray totLen
    offRef <- newSTRef 0
    for_ ss $ \sarr -> do
      let len = sizeofPrimArray sarr
      off <- readSTRef offRef
      copyPrimArray darr off sarr 0 len
      writeSTRef offRef (off + len)
    pure darr

-- | Concatenate multiple primitive arrays, each repeated a specified number of times.
concatRepsPrimArray :: (Prim a) => [(Int, PrimArray a)] -> PrimArray a
concatRepsPrimArray reps = runPrimArray $ do
  -- Calculate the total length required
  let calculateChunkLength (n, sarr) = n * sizeofPrimArray sarr
      totalLength = getSum (foldMap (Sum . calculateChunkLength) reps)

  -- Create the destination array
  destArr <- newPrimArray totalLength
  currentOffsetRef <- newSTRef 0

  -- Copy each repeated array into the destination array
  for_ reps $ \(n, sarr) -> do
    let srcSize = sizeofPrimArray sarr
    when (n > 0 && srcSize > 0) $ do
      currentOffset <- readSTRef currentOffsetRef
      -- Copy the source array 'n' times
      for_ [0 .. n - 1] $ \i -> do
        let destPos = currentOffset + i * srcSize
        copyPrimArray destArr destPos sarr 0 srcSize
      -- Update the offset for the next segment
      writeSTRef currentOffsetRef (currentOffset + n * srcSize)

  pure destArr

-- | Merge one primitive array into another using a combining function.
mergeIntoPrimArray
  :: (PrimMonad m, Prim a) => (a -> a -> a) -> MutablePrimArray (PrimState m) a -> Int -> PrimArray a -> Int -> Int -> m ()
mergeIntoPrimArray f darr doff sarr soff slen =
  for_ [0 .. slen - 1] $ \pos -> do
    let dpos = doff + pos
        spos = soff + pos
    val0 <- readPrimArray darr dpos
    let val1 = indexPrimArray sarr spos
    writePrimArray darr dpos (f val0 val1)

-- | Merge one mutable primitive array into another using a combining function.
mergeMutableIntoPrimArray
  :: (PrimMonad m, Prim a)
  => (a -> a -> a)
  -> MutablePrimArray (PrimState m) a
  -> Int
  -> MutablePrimArray (PrimState m) a
  -> Int
  -> Int
  -> m ()
mergeMutableIntoPrimArray f darr doff sarr soff slen =
  for_ [0 .. slen - 1] $ \pos -> do
    let dpos = doff + pos
        spos = soff + pos
    val0 <- readPrimArray darr dpos
    val1 <- readPrimArray sarr spos
    writePrimArray darr dpos (f val0 val1)

-- | Merge multiple primitive arrays using a combining function.
mergePrimArray :: (Prim a, Num a) => (a -> a -> a) -> [PrimArray a] -> PrimArray a
mergePrimArray f = \case
  [] -> emptyPrimArray
  [s0] -> s0
  ss -> runPrimArray $ do
    let totLen = getMax (foldMap (Max . sizeofPrimArray) ss)
    darr <- newPrimArray totLen
    setPrimArray darr 0 totLen 0
    for_ ss $ \sarr -> do
      let len = sizeofPrimArray sarr
      mergeIntoPrimArray f darr 0 sarr 0 len
    pure darr
