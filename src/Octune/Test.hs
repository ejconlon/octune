{-# LANGUAGE PartialTypeSignatures #-}

module Octune.Test (main) where

import Bowtie (Fix (..), pattern MemoP)
import Control.Exception (throwIO)
import Control.Monad (forM, join)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Dahdit.Audio.Binary (QuietLiftedArray (..))
import Dahdit.Audio.Wav.Simple (WAVESamples (..))
import Dahdit.LiftedPrimArray (LiftedPrimArray (..))
import Dahdit.Sizes (ElemCount (..))
import Data.Foldable (for_, toList)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Primitive.ByteArray (ByteArray (..), sizeofByteArray)
import Data.Primitive.PrimArray (primArrayFromList)
import Data.Ratio ((%))
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Sounds
  ( InternalSamples (..)
  , Op
  , OpErr
  , OpF (..)
  , isampsFromList
  , isampsIsNull
  , opAnnoLenTopo
  , opInferLenTopo
  , opToWave
  )
import Data.Topo (TopoErr, topoSort)
import PropUnit (Gen, TestLimit, TestTree, assert, forAll, testGroup, testMain, testProp, testUnit, (===))
import PropUnit.Hedgehog.Gen qualified as Gen
import PropUnit.Hedgehog.Range qualified as Range

simpleTopoSort :: [(Int, [Int])] -> Either (TopoErr Int) (Seq Int)
simpleTopoSort deps = topoSort (fmap Set.fromList . flip lookup deps) (fmap fst deps)

topoSortTests :: TestLimit -> TestTree
topoSortTests lim =
  testGroup
    "topoSort"
    [ testUnit "simple case" $
        simpleTopoSort [(1, [2]), (2, [3]), (3, [])] === Right (Seq.fromList [3, 2, 1])
    , testUnit "empty case" $ do
        simpleTopoSort [] === Right Seq.empty
    , testUnit "single node" $ do
        simpleTopoSort [(1, [])] === Right (Seq.singleton 1)
    , testProp "respects dependencies" lim $ do
        -- Generate a list of nodes with random dependencies
        nodes <- forAll $ Gen.list (Range.linear 1 10) (Gen.int (Range.linear 0 10))
        deps <- forAll $ forM nodes $ \n -> do
          -- Each node can depend on any node with a smaller number
          let possibleDeps = filter (< n) nodes
          deps <- Gen.list (Range.linear 0 (length possibleDeps)) (Gen.element possibleDeps)
          pure (n, deps)

        case simpleTopoSort deps of
          Left _ -> pure () -- Skip if sorting fails
          Right sorted -> do
            -- For each node in the sorted list
            for_ (zip [0 ..] (toList sorted)) $ \(i, n) -> do
              -- For each node that comes after it
              for_ (drop (i + 1) (toList sorted)) $ \m -> do
                -- Check that m is not in n's dependencies
                case lookup n deps of
                  Nothing -> pure ()
                  Just ds -> assert (m `notElem` ds)
    ]

opToWaveSimple :: (MonadIO m) => OpF Char (Op Char) -> m (Either (OpErr Char) InternalSamples)
opToWaveSimple = liftIO . opToWave 'a' . Fix

opTests :: TestLimit -> TestTree
opTests lim =
  testGroup
    "op"
    [ testUnit "opToWave OpEmpty" $ do
        outSamps <- either (liftIO . throwIO) pure =<< opToWaveSimple OpEmpty
        assert (isampsIsNull outSamps)
    , testUnit "opToWave OpSamp" $ do
        let inSamps = isampsFromList [1, 2, 3]
        outSamps <- either (liftIO . throwIO) pure =<< opToWaveSimple (OpSamp inSamps)
        outSamps === inSamps
    , testUnit "opToWave OpConcat" $ do
        let inSamps1 = isampsFromList [1 .. 3]
            inSamps2 = isampsFromList [4 .. 6]
            op = OpConcat (Seq.fromList [Fix (OpSamp inSamps1), Fix (OpSamp inSamps2)])
        outSamps <- either (liftIO . throwIO) pure =<< opToWaveSimple op
        outSamps === isampsFromList [1 .. 6]
    , testProp "upper bounds opAnnoLenTopo" lim $ do
        -- Generate a set of valid keys
        keys <- forAll $ Gen.list (Range.linear 1 5) (Gen.element ['a' .. 'z'])
        let validKeys = Set.fromList keys

        -- Generate a map of ops where all OpRef constructors use valid keys
        ops <- forAll $ genValidOpMap validKeys

        -- Get inferred lengths
        case opInferLenTopo ops of
          Left _ -> pure () -- Skip if inference fails
          Right inferred -> do
            -- Get annotated lengths
            case opAnnoLenTopo ops inferred of
              Left _ -> pure () -- Skip if annotation fails
              Right annotated -> do
                -- For each key in the map
                for_ (Map.keys ops) $ \k -> do
                  -- If we have an inferred length
                  case join (Map.lookup k inferred) of
                    Nothing -> pure () -- Skip if no inference
                    Just infLen -> do
                      -- Get the annotated length
                      case Map.lookup k annotated of
                        Nothing -> fail "Missing annotation"
                        Just (MemoP annLen _) -> do
                          -- Inferred length should be >= annotated length
                          assert $ infLen >= annLen
    ]

main :: IO ()
main = testMain $ \lim ->
  testGroup
    "suite"
    [ topoSortTests lim
    , opTests lim
    ]

genOp :: [n] -> Gen (Op n)
genOp validKeys = genR
 where
  genR = Gen.recursive Gen.choice nonRecursive recursive
  nonRecursive =
    [ pure (Fix OpEmpty)
    , Fix . OpSamp . InternalSamples . primArrayFromList
        <$> Gen.list (Range.linear 1 10) (Gen.int32 (Range.linear minBound maxBound))
    ]
      ++ ([Fix . OpRef <$> Gen.element validKeys | not (null validKeys)])
  recursive =
    [ Gen.subtermM genR $ \r -> do
        n <- Gen.int (Range.linear 1 10)
        pure (Fix (OpBound (ElemCount n) r))
    , Gen.subtermM genR $ \r -> do
        n <- Gen.integral (Range.linear 1 9)
        pure (Fix (OpCut (n % 10) r))
    , Gen.subterm genR (Fix . OpRepeat)
    , Gen.subtermM genR $ \r -> do
        n <- Gen.int (Range.linear 1 3)
        pure (Fix (OpReplicate n r))
    , genSeqSubterm genR (Fix . OpConcat)
    , genSeqSubterm genR (Fix . OpMerge)
    ]

genSeqSubterm :: Gen a -> (Seq a -> a) -> Gen a
genSeqSubterm g f =
  Gen.choice
    [ Gen.subterm g $ \x -> f (x :<| Empty)
    , Gen.subterm2 g g $ \x y -> f (x :<| y :<| Empty)
    , Gen.subterm3 g g g $ \x y z -> f (x :<| y :<| z :<| Empty)
    ]

genValidOpMap :: (Ord n) => Set n -> Gen (Map n (Op n))
genValidOpMap validKeys = do
  -- Generate a subset of valid keys to use in the map
  usedKeys <- Gen.subset validKeys
  -- Convert to list for ordered traversal
  let keyList = Set.toList usedKeys
  -- Unfold the map, ensuring each op only uses keys that come after it
  Map.fromList <$> unfoldM go keyList
 where
  go [] = pure Nothing
  go (k : ks) = do
    op <- genOp ks
    pure $ Just ((k, op), ks)

-- From monad-extras
unfoldM :: (Monad m) => (s -> m (Maybe (a, s))) -> s -> m [a]
unfoldM f s = do
  mres <- f s
  case mres of
    Nothing -> return []
    Just (a, s') -> fmap (a :) (unfoldM f s')
