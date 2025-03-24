module Octune.Test (main) where

import Control.Monad (join)
import Dahdit.Sizes (ElemCount (..))
import Data.Foldable (for_)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Primitive.PrimArray (primArrayFromList)
import Data.Ratio ((%))
import Data.Sequence (Seq (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Sounds (Anno (..), InternalSamples (..), Op (..), Op2Anno (..), OpF (..), opAnnoLenTopo, opInferLenTopo)
import PropUnit (Gen, assert, forAll, testGroup, testMain, testProp, testUnit, (===))
import PropUnit.Hedgehog.Gen qualified as Gen
import PropUnit.Hedgehog.Range qualified as Range

main :: IO ()
main = testMain $ \lim ->
  testGroup
    "suite"
    [ testUnit "Simple test example" $
        2 + 2 === (4 :: Int)
    , testProp "opInferLenTopo upper bounds opAnnoLenTopo" lim $ do
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
                        Just (Op2Anno (Anno annLen _)) -> do
                          -- Inferred length should be >= annotated length
                          assert $ infLen >= annLen
    ]

genOp :: [n] -> Gen (Op n)
genOp validKeys = genR
 where
  genR = Gen.recursive Gen.choice nonRecursive recursive
  nonRecursive =
    [ pure (Op OpEmpty)
    , Op . OpSamp . InternalSamples . primArrayFromList
        <$> Gen.list (Range.linear 1 10) (Gen.int32 (Range.linear minBound maxBound))
    ]
      ++ ([Op . OpRef <$> Gen.element validKeys | not (null validKeys)])
  recursive =
    [ Gen.subtermM genR $ \r -> do
        n <- Gen.int (Range.linear 1 10)
        pure (Op (OpBound (ElemCount n) r))
    , Gen.subtermM genR $ \r -> do
        n <- Gen.integral (Range.linear 1 9)
        pure (Op (OpCut (n % 10) r))
    , Gen.subterm genR (Op . OpRepeat)
    , Gen.subtermM genR $ \r -> do
        n <- Gen.int (Range.linear 1 3)
        pure (Op (OpReplicate n r))
    , genSeqSubterm genR (Op . OpConcat)
    , genSeqSubterm genR (Op . OpMerge)
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
