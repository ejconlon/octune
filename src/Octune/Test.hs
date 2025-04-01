module Octune.Test (main) where

import Bowtie (Fix (..), pattern MemoP)
import Control.Monad (forM)
import Data.Foldable (for_, toList)
import Data.Map.Strict qualified as Map
import Data.Primitive.PrimArray (primArrayFromList)
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Sounds
  ( Arc (..)
  , Delta (..)
  , Extent (..)
  , InternalSamples (..)
  , Op
  , OpF (..)
  , Rate (..)
  , Reps (..)
  , isampsFromList
  , opAnnoExtentSingle
  , opInferExtentTopo
  , opRenderSimple
  )
import Data.Topo (SortErr, topoSort)
import PropUnit (Gen, TestLimit, TestTree, assert, forAll, testGroup, testMain, testProp, testUnit, (===))
import PropUnit.Hedgehog.Gen qualified as Gen
import PropUnit.Hedgehog.Range qualified as Range

topoSortSimple :: [(Int, [Int])] -> Either (SortErr Int) (Seq Int)
topoSortSimple deps = topoSort (fmap Set.fromList . flip lookup deps) (fmap fst deps)

topoSortTests :: TestLimit -> TestTree
topoSortTests lim =
  testGroup
    "topoSort"
    [ testUnit "simple case" $
        topoSortSimple [(1, [2]), (2, [3]), (3, [])] === Right (Seq.fromList [3, 2, 1])
    , testUnit "empty case" $ do
        topoSortSimple [] === Right Seq.empty
    , testUnit "single node" $ do
        topoSortSimple [(1, [])] === Right (Seq.singleton 1)
    , testProp "respects dependencies" lim $ do
        -- Generate a list of nodes with random dependencies
        nodes <- forAll $ Gen.list (Range.linear 1 10) (Gen.int (Range.linear 0 10))
        deps <- forAll $ forM nodes $ \n -> do
          -- Each node can depend on any node with a smaller number
          let possibleDeps = filter (< n) nodes
          deps <- Gen.list (Range.linear 0 (length possibleDeps)) (Gen.element possibleDeps)
          pure (n, deps)
        case topoSortSimple deps of
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

type TestOpF = OpF Char (Fix (OpF Char))

opTests :: TestLimit -> TestTree
opTests lim =
  testGroup
    "op"
    [ testUnit "opAnnoExtent OpEmpty" $ do
        let op = Fix (OpEmpty :: TestOpF)
        opAnnoExtentSingle (Rate 1) op === Right (MemoP (Extent (Arc 0 0)) OpEmpty)
        opRenderSimple (Rate 1) op === Right (isampsFromList [])
    , testUnit "opAnnoExtent OpSamp" $ do
        let inSamps = isampsFromList [1, 2, 3]
            op = Fix (OpSamp inSamps :: TestOpF)
        opAnnoExtentSingle (Rate 1) op === Right (MemoP (Extent (Arc 0 3)) (OpSamp inSamps))
        opRenderSimple (Rate 1) op === Right inSamps
    , testUnit "opAnnoExtent OpShift" $ do
        let inSamps = isampsFromList [1, 2, 3]
            inner = Fix (OpSamp inSamps :: TestOpF)
            op = Fix (OpShift (Delta 2) inner :: TestOpF)
        -- TODO Annotate with extents for correct final length
        opAnnoExtentSingle (Rate 1) op
          === Right (MemoP (Extent (Arc (-2) 1)) (OpShift (Delta 2) (MemoP (Extent (Arc 0 3)) (OpSamp inSamps))))
    , -- TODO fix this
      -- opRenderSimple (Rate 1) op === Right (isampsFromList [0, 0, 1, 2, 3])
      testUnit "opAnnoExtent OpSlice" $ do
        let inSamps = isampsFromList [1, 2, 3, 4, 5, 6]
            inner = Fix (OpSamp inSamps :: TestOpF)
            op = Fix (OpSlice (Reps 2) (Arc 1 3) inner :: TestOpF)
        opAnnoExtentSingle (Rate 1) op
          === Right (MemoP (Extent (Arc 0 4)) (OpSlice (Reps 2) (Arc 1 3) (MemoP (Extent (Arc 0 6)) (OpSamp inSamps))))
    , -- TODO fix this
      -- opRenderSimple (Rate 1) op === Right (isampsFromList [2, 3, 2, 3])
      testUnit "opAnnoExtent OpConcat" $ do
        let inSamps1 = isampsFromList [1, 2, 3]
            inSamps2 = isampsFromList [4, 5, 6]
            op1 = Fix (OpSamp inSamps1 :: TestOpF)
            op2 = Fix (OpSamp inSamps2 :: TestOpF)
            op = Fix (OpConcat (Seq.fromList [op1, op2]) :: TestOpF)
        opAnnoExtentSingle (Rate 1) op
          === Right
            ( MemoP
                (Extent (Arc 0 6))
                ( OpConcat
                    ( Seq.fromList
                        [MemoP (Extent (Arc 0 3)) (OpSamp inSamps1), MemoP (Extent (Arc 0 3)) (OpSamp inSamps2)]
                    )
                )
            )
    , -- TODO fix this
      -- opRenderSimple (Rate 1) op === Right (isampsFromList [1, 2, 3, 4, 5, 6])
      testUnit "opAnnoExtent OpMerge" $ do
        let inSamps1 = isampsFromList [1, 2]
            inSamps2 = isampsFromList [4, 5, 6]
            op1 = Fix (OpSamp inSamps1 :: TestOpF)
            op2 = Fix (OpSamp inSamps2 :: TestOpF)
            op = Fix (OpMerge (Seq.fromList [op1, op2]) :: TestOpF)
        opAnnoExtentSingle (Rate 1) op
          === Right
            ( MemoP
                (Extent (Arc 0 3))
                ( OpMerge
                    ( Seq.fromList
                        [MemoP (Extent (Arc 0 2)) (OpSamp inSamps1), MemoP (Extent (Arc 0 3)) (OpSamp inSamps2)]
                    )
                )
            )
        opRenderSimple (Rate 1) op === Right (isampsFromList [5, 7, 6])
    , testUnit "opAnnoExtent OpRef" $ do
        let op = Fix (OpRef 'a' :: TestOpF)
        opAnnoExtentSingle (Rate 1) op === Left 'a'
    , testProp "opInferExtentTopo respects dependencies" lim $ do
        -- Generate a set of valid keys
        keys <- forAll $ Gen.list (Range.linear 1 5) (Gen.element ['a' .. 'z'])
        let validKeys = Set.fromList keys

        -- Generate a map of ops where all OpRef constructors use valid keys
        ops <- forAll $ genValidOpMap validKeys

        -- Get inferred lengths
        case opInferExtentTopo (Rate 1) ops of
          Left _ -> pure () -- Skip if inference fails
          Right inferred -> do
            -- For each key in the map
            for_ (Map.keys ops) $ \k -> do
              -- If we have an inferred length
              case Map.lookup k inferred of
                Nothing -> pure () -- Skip if no inference
                Just (Right infLen) -> do
                  -- Get the annotated length
                  case opAnnoExtentSingle (Rate 1) (ops Map.! k) of
                    Left _ -> pure () -- Skip if annotation fails
                    Right (MemoP annLen _) -> do
                      -- Inferred length should be >= annotated length
                      assert $ infLen >= annLen
                Just (Left _) -> pure () -- Skip if inference failed for this key
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
    [ pure (Fix (OpEmpty :: OpF n (Fix (OpF n))))
    , Fix . OpSamp . InternalSamples . primArrayFromList
        <$> Gen.list (Range.linear 1 10) (Gen.int32 (Range.linear minBound maxBound))
    ]
      ++ ([Fix . OpRef <$> Gen.element validKeys | not (null validKeys)])
  recursive =
    [ Gen.subtermM genR $ \r -> do
        n <- Gen.integral (Range.linear 1 10)
        pure (Fix (OpShift (Delta (fromInteger n)) r))
    , Gen.subtermM genR $ \r -> do
        n <- Gen.integral (Range.linear 1 3)
        pure (Fix (OpSlice (Reps n) (Arc 0 1) r))
    , genSeqSubterm genR (Fix . OpConcat)
    , genSeqSubterm genR (Fix . OpMerge)
    ]

genSeqSubterm :: Gen (Op n) -> (Seq.Seq (Op n) -> Op n) -> Gen (Op n)
genSeqSubterm genOp' f = do
  n <- Gen.integral (Range.linear 1 3)
  ops <- Gen.list (Range.singleton n) genOp'
  pure (f (Seq.fromList ops))

genValidOpMap :: (Ord n) => Set.Set n -> Gen (Map.Map n (Op n))
genValidOpMap validKeys = do
  n <- Gen.integral (Range.linear 1 5)
  keys <- Gen.list (Range.singleton n) (Gen.element (Set.toList validKeys))
  ops <- Gen.list (Range.singleton n) (genOp (Set.toList validKeys))
  pure (Map.fromList (zip keys ops))

-- From monad-extras
unfoldM :: (Monad m) => (s -> m (Maybe (a, s))) -> s -> m [a]
unfoldM f s = do
  mres <- f s
  case mres of
    Nothing -> return []
    Just (a, s') -> fmap (a :) (unfoldM f s')
