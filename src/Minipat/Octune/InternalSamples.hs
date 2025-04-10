module Minipat.Octune.InternalSamples where

import Control.DeepSeq (NFData)
import Control.Monad (when)
import Dahdit (ByteCount (..), ElemCount (..), LiftedPrimArray (..))
import Dahdit.Audio.Binary (QuietLiftedArray (..))
import Dahdit.Audio.Wav.Simple (WAVESamples (..))
import Data.Bifunctor (second)
import Data.Int (Int32)
import Data.Primitive.ByteArray (ByteArray (..))
import Data.Primitive.PrimArray
  ( PrimArray (..)
  , clonePrimArray
  , copyPrimArray
  , emptyPrimArray
  , generatePrimArray
  , indexPrimArray
  , mapPrimArray
  , newPrimArray
  , primArrayFromList
  , replicatePrimArray
  , runPrimArray
  , setPrimArray
  , sizeofPrimArray
  )
import Minipat.Octune.Util (concatPrimArray, concatRepsPrimArray, mergePrimArray, replicateWholePrimArray)

-- | Convert a byte count to an element count for Int32 arrays. (Mono only)
i32ByteToElemCount :: ByteCount -> ElemCount
i32ByteToElemCount = ElemCount . (4 *) . unByteCount

-- | Convert an element count to a byte count for Int32 arrays. (Mono only)
i32ElemToByteCount :: ElemCount -> ByteCount
i32ElemToByteCount = ByteCount . (`div` 4) . unElemCount

newtype InternalSamples = InternalSamples {unInternalSamples :: PrimArray Int32}
  deriving stock (Eq, Show)
  deriving newtype (NFData)

-- | Create an empty samples array.
isampsEmpty :: InternalSamples
isampsEmpty = InternalSamples emptyPrimArray

-- | Check if an samples array is empty.
isampsNull :: InternalSamples -> Bool
isampsNull = (0 ==) . isampsBytes

-- | Get the length of an samples array in elements.
isampsLength :: InternalSamples -> ElemCount
isampsLength = ElemCount . sizeofPrimArray . unInternalSamples

-- | Get the length of an samples array in bytes.
isampsBytes :: InternalSamples -> ByteCount
isampsBytes = i32ElemToByteCount . isampsLength

-- | Get an element from an samples array.
isampsIndex :: InternalSamples -> ElemCount -> Int32
isampsIndex s = indexPrimArray (unInternalSamples s) . unElemCount

-- | Create an samples array filled with a constant value.
isampsConstant :: ElemCount -> Int32 -> InternalSamples
isampsConstant len = InternalSamples . replicatePrimArray (unElemCount len)

-- | Replicate an samples array a given number of times.
isampsReplicate :: Int -> InternalSamples -> InternalSamples
isampsReplicate n = InternalSamples . replicateWholePrimArray n . unInternalSamples

-- | Create an samples array from a list.
isampsFromList :: [Int32] -> InternalSamples
isampsFromList = InternalSamples . primArrayFromList

-- | Concatenate multiple samples arrays, each repeated a specified number of times.
isampsConcatReps :: [(Int, InternalSamples)] -> InternalSamples
isampsConcatReps = InternalSamples . concatRepsPrimArray . fmap (second unInternalSamples)

-- | Concatenate multiple samples arrays.
isampsConcat :: [InternalSamples] -> InternalSamples
isampsConcat = InternalSamples . concatPrimArray . fmap unInternalSamples

-- | Mix multiple internal arrays by adding their values.
isampsMix :: [InternalSamples] -> InternalSamples
isampsMix = InternalSamples . mergePrimArray (+) . fmap unInternalSamples

-- | Extract a sub-range from an samples array.
isampsTrim :: ElemCount -> ElemCount -> InternalSamples -> InternalSamples
isampsTrim off len s = InternalSamples (clonePrimArray (unInternalSamples s) (unElemCount off) (unElemCount len))

-- | Map a function over an samples array.
isampsMap :: (Int32 -> Int32) -> InternalSamples -> InternalSamples
isampsMap f = InternalSamples . mapPrimArray f . unInternalSamples

-- | Fill a sub-range of an samples array with a constant value.
isampsFill :: ElemCount -> ElemCount -> Int32 -> InternalSamples -> InternalSamples
isampsFill off len val (InternalSamples sarr) = InternalSamples $ runPrimArray $ do
  let tot = ElemCount (sizeofPrimArray sarr)
      lim = off + len
      left = tot - lim
  darr <- newPrimArray (unElemCount tot)
  when (off > 0) (copyPrimArray darr 0 sarr 0 (unElemCount off))
  setPrimArray darr (unElemCount off) (unElemCount len) val
  when (left > 0) (copyPrimArray darr (unElemCount lim) sarr (unElemCount lim) (unElemCount left))
  pure darr

-- | Convert samples to WAV format.
isampsToWave :: InternalSamples -> WAVESamples
isampsToWave = WAVESamples . QuietLiftedArray . LiftedPrimArray . (\(PrimArray x) -> ByteArray x) . unInternalSamples

-- | Convert WAV format to samples.
isampsFromWave :: WAVESamples -> InternalSamples
isampsFromWave = InternalSamples . (\(ByteArray x) -> PrimArray x) . unLiftedPrimArray . unQuietLiftedArray . unWAVESamples

-- | A stream of samples that can be repeated.
data SampleStream = SampleStream
  { ssFixed :: !InternalSamples
  -- ^ The fixed portion of the stream
  , ssRepeated :: !InternalSamples
  -- ^ The repeated portion of the stream
  }
  deriving stock (Eq, Show)

-- | Run a sample stream at a given position.
streamRun :: SampleStream -> ElemCount -> Int32
streamRun (SampleStream fixed repeated) =
  let flen = isampsLength fixed
      rlen = isampsLength repeated
  in  \pos ->
        if
          | pos < 0 -> 0
          | pos < flen ->
              indexPrimArray (unInternalSamples fixed) (unElemCount pos)
          | rlen == 0 -> 0
          | otherwise ->
              indexPrimArray (unInternalSamples repeated) (unElemCount (mod (pos - flen) rlen))

-- | Convert a sample stream to samples.
streamToIsamps :: ElemCount -> ElemCount -> SampleStream -> InternalSamples
streamToIsamps off len t =
  InternalSamples (generatePrimArray (unElemCount len) (streamRun t . (off +) . ElemCount))
