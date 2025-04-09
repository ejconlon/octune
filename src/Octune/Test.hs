module Octune.Test (main) where

import Bowtie (Fix (..), memoKey, pattern MemoP)
import Control.Exception (evaluate, throwIO)
import Control.Monad (forM)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Foldable (for_, toList)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
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
  , Overlap (..)
  , QTime (..)
  , Rate (..)
  , Samples (..)
  , Time (..)
  , arcDeltaCover
  , arcDeltaCoverMax
  , arcEmpty
  , arcLen
  , arcOverlap
  , arcRelative
  , arcShift
  , extentPosArc
  , isampsEmpty
  , isampsFromList
  , isampsLength
  , opAnnoExtentSingle
  , opAnnoExtentTopo
  , opRenderMutSingle
  , opRenderMutSingleOn
  , opRenderMutTopo
  , opRenderSingle
  , opRenderSingleOn
  , opRenderTopo
  , quantizeArc
  , runMutSamplesSimple
  , unquantizeArc
  )
import Data.Topo (SortErr, topoSort)
import Data.Traversable (for)
import PropUnit (Gen, PropertyT, TestLimit, TestTree, assert, forAll, testGroup, testMain, testProp, testUnit, (===))
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

utilTests :: TestLimit -> TestTree
utilTests lim =
  testGroup
    "util"
    [ testUnit "arcRelative empty" $ do
        arcRelative @Time (Arc 0 0) (Arc 0 2) === Nothing
        arcRelative @Time (Arc 1 1) (Arc 0 2) === Nothing
        arcRelative @Time (Arc 2 2) (Arc 0 2) === Nothing
    , testUnit "arcRelative exact match" $ do
        arcRelative @Time (Arc 0 1) (Arc 0 1) === Just (0, Arc 0 1, 0)
        arcRelative @Time (Arc 0 1) (Arc 10 11) === Just (0, Arc 10 11, 0)
    , testUnit "arcRelative contained" $ do
        arcRelative @Time (Arc 1 2) (Arc 0 3) === Just (0, Arc 1 2, 0)
        arcRelative @Time (Arc 1 2) (Arc 10 13) === Just (0, Arc 11 12, 0)
    , testUnit "arcRelative overlapping start" $ do
        arcRelative @Time (Arc (-1) 1) (Arc 0 2) === Just (1, Arc 0 1, 0)
        arcRelative @Time (Arc (-1) 1) (Arc 10 12) === Just (1, Arc 10 11, 0)
    , testUnit "arcRelative overlapping end" $ do
        arcRelative @Time (Arc 1 3) (Arc 0 2) === Just (0, Arc 1 2, 1)
        arcRelative @Time (Arc 1 3) (Arc 10 12) === Just (0, Arc 11 12, 1)
    , testUnit "arcRelative completely before" $ do
        arcRelative @Time (Arc (-2) (-1)) (Arc 0 1) === Nothing
        arcRelative @Time (Arc (-2) (-1)) (Arc 10 11) === Nothing
    , testUnit "arcRelative completely after" $ do
        arcRelative @Time (Arc 2 3) (Arc 0 1) === Nothing
        arcRelative @Time (Arc 2 3) (Arc 10 11) === Nothing
    , testUnit "arcRelative containing" $ do
        arcRelative @Time (Arc (-1) 4) (Arc 0 3) === Just (1, Arc 0 3, 1)
        arcRelative @Time (Arc (-1) 4) (Arc 10 13) === Just (1, Arc 10 13, 1)
    , testUnit "arcOverlap empty" $ do
        arcOverlap @Time (Arc 0 0) (Arc 0 2) === OverlapLt
        arcOverlap @Time (Arc 1 1) (Arc 0 2) === OverlapOn 0 (Arc 1 1) 0
        arcOverlap @Time (Arc 2 2) (Arc 0 2) === OverlapGt
    , testUnit "arcOverlap exact match" $ do
        arcOverlap @Time (Arc 0 1) (Arc 0 1) === OverlapOn 0 (Arc 0 1) 0
        arcOverlap @Time (Arc 0 1) (Arc 10 11) === OverlapLt
    , testUnit "arcOverlap contained" $ do
        arcOverlap @Time (Arc 1 2) (Arc 0 3) === OverlapOn 0 (Arc 1 2) 0
        arcOverlap @Time (Arc 1 2) (Arc 10 13) === OverlapLt
    , testUnit "arcOverlap overlapping start" $ do
        arcOverlap @Time (Arc (-1) 1) (Arc 0 2) === OverlapOn 1 (Arc 0 1) 0
        arcOverlap @Time (Arc (-1) 1) (Arc 10 12) === OverlapLt
    , testUnit "arcOverlap overlapping end" $ do
        arcOverlap @Time (Arc 1 3) (Arc 0 2) === OverlapOn 0 (Arc 1 2) 1
        arcOverlap @Time (Arc 1 3) (Arc 10 12) === OverlapLt
    , testUnit "arcOverlap completely before" $ do
        arcOverlap @Time (Arc (-2) (-1)) (Arc 0 1) === OverlapLt
        arcOverlap @Time (Arc (-2) (-1)) (Arc 10 11) === OverlapLt
    , testUnit "arcOverlap completely after" $ do
        arcOverlap @Time (Arc 2 3) (Arc 0 1) === OverlapGt
        arcOverlap @Time (Arc 2 3) (Arc 10 11) === OverlapLt
    , testUnit "arcOverlap containing" $ do
        arcOverlap @Time (Arc (-1) 4) (Arc 0 3) === OverlapOn 1 (Arc 0 3) 1
        arcOverlap @Time (Arc (-1) 4) (Arc 10 13) === OverlapLt
    , testUnit "arcDeltaCover empty" $ do
        arcDeltaCover @Time 1 (Arc 0 0) === Nothing
        arcDeltaCover @Time 1 (Arc 1 1) === Nothing
    , testUnit "arcDeltaCover exact match" $ do
        arcDeltaCover @Time 1 (Arc 0 1) === Just (0, 0, 1)
        arcDeltaCover @Time 1 (Arc 1 2) === Just (0, 1, 2)
    , testUnit "arcDeltaCover negative" $ do
        arcDeltaCover @Time 1 (Arc (-0.5) 0.5) === Just (0.5, 0, 1)
        arcDeltaCover @Time 1 (Arc (-1.5) 1.5) === Just (1.5, 0, 2)
    , testUnit "arcDeltaCover spanning" $ do
        arcDeltaCover @Time 1 (Arc 0.5 2.5) === Just (0, 0, 3)
        arcDeltaCover @Time 1 (Arc (-0.5) 2.5) === Just (0.5, 0, 3)
    , testUnit "arcDeltaCover non-positive delta" $ do
        arcDeltaCover @Time 0 (Arc 0 1) === Nothing
        arcDeltaCover @Time (-1) (Arc 0 1) === Nothing
    , testUnit "arcDeltaCoverMax basic" $ do
        arcDeltaCoverMax @Time 1 2 (Arc 0 1) === Just (0, 0, 1, 0)
        arcDeltaCoverMax @Time 1 2 (Arc 0 2) === Just (0, 0, 2, 0)
        arcDeltaCoverMax @Time 1 2 (Arc 0 3) === Just (0, 0, 2, 1)
        arcDeltaCoverMax @Time 1 2 (Arc 0 4) === Just (0, 0, 2, 2)
    , testUnit "arcDeltaCoverMax negative" $ do
        arcDeltaCoverMax @Time 1 2 (Arc (-1) 1) === Just (1, 0, 1, 0)
        arcDeltaCoverMax @Time 1 2 (Arc (-1) 2) === Just (1, 0, 2, 0)
        arcDeltaCoverMax @Time 1 2 (Arc (-1) 3) === Just (1, 0, 2, 1)
    , testUnit "arcDeltaCoverMax non-positive delta" $ do
        arcDeltaCoverMax @Time 0 2 (Arc 0 1) === Nothing
        arcDeltaCoverMax @Time (-1) 2 (Arc 0 1) === Nothing
    , testUnit "arcDeltaCoverMax non-positive reps" $ do
        arcDeltaCoverMax @Time 1 0 (Arc 0 1) === Nothing
        arcDeltaCoverMax @Time 1 (-1) (Arc 0 1) === Nothing
    , testUnit "arcDeltaCoverMax empty arc" $ do
        arcDeltaCoverMax @Time 1 2 (Arc 0 0) === Nothing
        arcDeltaCoverMax @Time 1 2 (Arc 1 1) === Nothing
    , testUnit "arcDeltaCoverMax fractional reps" $ do
        arcDeltaCoverMax @Time 1 1.5 (Arc 0 2) === Just (0, 0, 2, 0.5)
        arcDeltaCoverMax @Time 1 2.5 (Arc 0 3) === Just (0, 0, 3, 0.5)
    , testProp "prop quantize shift invariance" lim $ do
        let genReal = Gen.realFrac_ (Range.linearFracFrom 0 (-100) 100)
            genSmallPosReal = Gen.realFrac_ (Range.linearFrac 1 10)
        rate <- forAll (fmap Rate genSmallPosReal)
        a0 <- forAll (fmap Time genReal)
        b0 <- forAll (fmap Time genReal)
        let a = min a0 b0
            b = max a0 b0
            arc = Arc a b
        d <- forAll (fmap Delta genReal)
        let qbefore = quantizeArc rate arc
            lbefore = arcLen qbefore
        let qafter = quantizeArc rate (arcShift arc d)
            lafter = arcLen qafter
        lafter === lbefore
    , testProp "prop quantize inverse 1" lim $ do
        let genInt = Gen.integral (Range.linearFrom 0 (-100) 100)
            genSmallPosReal = Gen.realFrac_ (Range.linearFrac 1 10)
        rate <- forAll (fmap Rate genSmallPosReal)
        a0 <- forAll (fmap (Time . (/ unRate rate) . fromInteger) genInt)
        b0 <- forAll (fmap (Time . (/ unRate rate) . fromInteger) genInt)
        let arc = Arc a0 b0
        let q = quantizeArc rate arc
        let u = unquantizeArc rate q
        u === arc
    , testProp "prop quantize inverse 2" lim $ do
        let genInt = Gen.integral (Range.linearFrom 0 (-100) 100)
            genSmallPosReal = Gen.realFrac_ (Range.linearFrac 1 10)
        rate <- forAll (fmap Rate genSmallPosReal)
        a0 <- forAll (fmap QTime genInt)
        b0 <- forAll (fmap QTime genInt)
        let arc = Arc a0 b0
        let u = unquantizeArc rate arc
        let q = quantizeArc rate u
        q === arc
    , testUnit "quantize cases" $ do
        let rate = Rate 1
        -- Test empty arcs
        quantizeArc rate (Arc 0 0) === Arc 0 0
        quantizeArc rate (Arc 1 1) === Arc 1 1
        quantizeArc rate (Arc (-1) (-1)) === Arc (-1) (-1)

        -- Test basic positive arcs
        quantizeArc rate (Arc 0 1) === Arc 0 1
        quantizeArc rate (Arc 0.1 1.1) === Arc 0 1
        quantizeArc rate (Arc 0.9 1.9) === Arc 0 1

        -- Test basic negative arcs
        quantizeArc rate (Arc (-1) 0) === Arc (-1) 0
        quantizeArc rate (Arc (-1.1) (-0.1)) === Arc (-2) (-1)
        quantizeArc rate (Arc (-1.9) (-0.9)) === Arc (-2) (-1)

        -- Test shift invariance
        let arc1 = Arc (0 :: Time) 1
            arc2 = Arc ((-1) :: Time) 0
            arc3 = Arc (0.5 :: Time) 1.5
            arc4 = Arc ((-0.5) :: Time) 0.5
            qarc1 = quantizeArc rate arc1
            qarc2 = quantizeArc rate arc2
            qarc3 = quantizeArc rate arc3
            qarc4 = quantizeArc rate arc4
            len1 = arcLen qarc1
            len2 = arcLen qarc2
            len3 = arcLen qarc3
            len4 = arcLen qarc4
        len1 === len2
        len3 === len4

        -- Test fractional lengths
        quantizeArc rate (Arc 0 0.5) === Arc 0 1
        quantizeArc rate (Arc 0 1.5) === Arc 0 2
        quantizeArc rate (Arc (-0.5) 0) === Arc (-1) 0
        quantizeArc rate (Arc (-1.5) 0) === Arc (-2) 0

        -- Test arcs spanning zero
        quantizeArc rate (Arc (-0.7) 0.7) === Arc (-1) 1
        quantizeArc rate (Arc (-1.2) 1.2) === Arc (-2) 1
    ]

opTests :: TestLimit -> TestTree
opTests lim =
  testGroup
    "op"
    [ testUnit "OpEmpty" $ do
        let op = Fix (OpEmpty :: TestOpF)
        opAnnoExtentSingle (Rate 1) op === Right (MemoP (Extent (Arc 0 0)) OpEmpty)
        let expected = Right isampsEmpty
        opRenderSingle (Rate 1) op === expected
        liftIO (opRenderMutSingle (Rate 1) op) >>= (=== expected)
    , testUnit "OpSamp" $ do
        let inSamps = isampsFromList [1, 2, 3]
            op = Fix (OpSamp inSamps :: TestOpF)
        opAnnoExtentSingle (Rate 1) op === Right (MemoP (Extent (Arc 0 3)) (OpSamp inSamps))
        let expected = Right inSamps
        opRenderSingle (Rate 1) op === expected
        liftIO (opRenderMutSingle (Rate 1) op) >>= (=== expected)
    , testUnit "OpShift" $ do
        let inSamps = isampsFromList [1, 2, 3]
            inner = Fix (OpSamp inSamps :: TestOpF)
            op = Fix (OpShift (Delta 2) inner :: TestOpF)
        opAnnoExtentSingle (Rate 1) op
          === Right (MemoP (Extent (Arc (-2) 1)) (OpShift (Delta 2) (MemoP (Extent (Arc 0 3)) (OpSamp inSamps))))
        let expected = Right (isampsFromList [3])
        opRenderSingle (Rate 1) op === expected
        liftIO (opRenderMutSingle (Rate 1) op) >>= (=== expected)
    , testUnit "OpShift negative" $ do
        let inSamps = isampsFromList [1, 2, 3]
            inner = Fix (OpSamp inSamps :: TestOpF)
            op = Fix (OpShift (Delta (-2)) inner :: TestOpF)
        opAnnoExtentSingle (Rate 1) op
          === Right (MemoP (Extent (Arc 2 5)) (OpShift (Delta (-2)) (MemoP (Extent (Arc 0 3)) (OpSamp inSamps))))
        let expected = Right (isampsFromList [0, 0, 1, 2, 3])
        opRenderSingle (Rate 1) op === expected
        liftIO (opRenderMutSingle (Rate 1) op) >>= (=== expected)
    , testUnit "OpSlice" $ do
        let inSamps = isampsFromList [1, 2, 3, 4, 5, 6]
            inner = Fix (OpSamp inSamps :: TestOpF)
            op = Fix (OpSlice (Arc 1 3) inner :: TestOpF)
        opAnnoExtentSingle (Rate 1) op
          === Right (MemoP (Extent (Arc 0 2)) (OpSlice (Arc 1 3) (MemoP (Extent (Arc 0 6)) (OpSamp inSamps))))
        let expected = Right (isampsFromList [2, 3])
        opRenderSingle (Rate 1) op === expected
        liftIO (opRenderMutSingle (Rate 1) op) >>= (=== expected)
    , testUnit "OpRepeat" $ do
        let inSamps = isampsFromList [1, 2, 3]
            op = Fix (OpRepeat 2 (Fix (OpSamp inSamps)) :: TestOpF)
        opAnnoExtentSingle (Rate 1) op
          === Right (MemoP (Extent (Arc 0 6)) (OpRepeat 2 (MemoP (Extent (Arc 0 3)) (OpSamp inSamps))))
        let expected = Right (isampsFromList [1, 2, 3, 1, 2, 3])
        opRenderSingle (Rate 1) op === expected
        liftIO (opRenderMutSingle (Rate 1) op) >>= (=== expected)
    , testUnit "OpConcat" $ do
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
        let expected = Right (isampsFromList [1, 2, 3, 4, 5, 6])
        opRenderSingle (Rate 1) op === expected
        liftIO (opRenderMutSingle (Rate 1) op) >>= (=== expected)
    , testUnit "OpMerge" $ do
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
        let expected = Right (isampsFromList [5, 7, 6])
        opRenderSingle (Rate 1) op === expected
        liftIO (opRenderMutSingle (Rate 1) op) >>= (=== expected)
    , testUnit "OpRef" $ do
        let op = Fix (OpRef 'a' :: TestOpF)
        opAnnoExtentSingle (Rate 1) op === Left 'a'
    , testProp "gen test" lim $ do
        let rate = Rate 1
        -- Generate a set of valid keys
        keys <- forAll (Gen.list (Range.linear 1 5) (Gen.element ['a' .. 'z']))
        -- let keys = ['a']
        let validKeys = Set.fromList keys
        -- Generate a map of ops with valid references
        ops <- forAll (genValidOpMap validKeys)
        -- Infer and annotate lengths
        case opAnnoExtentTopo rate ops of
          Left err -> liftIO (throwIO err)
          Right ans -> do
            case sequence ans of
              Left n -> fail ("Missing key " ++ show n)
              Right ans' -> do
                renderRes <- either (liftIO . throwIO) pure (opRenderTopo rate ans')
                renderMutRes <- either (liftIO . throwIO) pure (opRenderMutTopo rate ans')
                for_ (Map.toList renderRes) $ \(k, samps) -> do
                  let an = ans' Map.! k
                      ex = memoKey an
                      mutSamps = renderMutRes Map.! k
                  case extentPosArc ex of
                    Nothing -> pure ()
                    Just arc -> do
                      let qArc = quantizeArc rate arc
                          qLen = arcLen qArc
                      arr <- liftIO (evaluate (runSamples samps qArc))
                      fromIntegral (isampsLength arr) === qLen
                      marr <- liftIO (evaluate =<< runMutSamplesSimple rate mutSamps qArc)
                      fromIntegral (isampsLength marr) === qLen
                      marr === arr
    , testProp "invar concat left" lim $ do
        let opE = Fix OpEmpty
        opG <- forAll (genOp @Char [])
        let opBase = Fix (OpConcat (Seq.fromList [opE, opG]))
        assertRenderSame opBase opG
    , testProp "invar concat right" lim $ do
        let opE = Fix OpEmpty
        opG <- forAll (genOp @Char [])
        let opBase = Fix (OpConcat (Seq.fromList [opG, opE]))
        assertRenderSame opBase opG
    , testProp "invar merge left" lim $ do
        let opE = Fix OpEmpty
        opG <- forAll (genOp @Char [])
        let opBase = Fix (OpMerge (Seq.fromList [opE, opG]))
        assertRenderSame opBase opG
    , testProp "invar merge right" lim $ do
        let opE = Fix OpEmpty
        opG <- forAll (genOp @Char [])
        let opBase = Fix (OpMerge (Seq.fromList [opG, opE]))
        assertRenderSame opBase opG
    ]

assertRenderSame :: (Eq n, Show n) => Op n -> Op n -> PropertyT IO ()
assertRenderSame opBase opMod = do
  let rate = Rate 1
  an <- either (const (fail "impossible")) pure (opAnnoExtentSingle rate opBase)
  let arc = fromMaybe (arcEmpty @Time) (extentPosArc (memoKey an))
  let irBase = opRenderSingleOn rate opBase arc
      irMod = opRenderSingleOn rate opMod arc
  irMod === irBase
  mrBase <- liftIO (opRenderMutSingleOn rate opBase arc)
  mrMod <- liftIO (opRenderMutSingleOn rate opMod arc)
  mrBase === mrMod

main :: IO ()
main = testMain $ \lim ->
  testGroup
    "suite"
    [ topoSortTests lim
    , utilTests lim
    , opTests lim
    ]

genOp :: [n] -> Gen (Op n)
genOp validKeys = genR
 where
  genR = Gen.recursive Gen.choice nonRecursive recursive
  nonRecursive =
    [ pure (Fix (OpEmpty :: OpF n (Fix (OpF n))))
    , Fix . OpSamp . InternalSamples . primArrayFromList
        <$> Gen.list (Range.linear 1 10) (Gen.int32 (Range.linear 0 100))
    ]
      ++ ([Fix . OpRef <$> Gen.element validKeys | not (null validKeys)])
  recursive =
    [ Gen.subtermM genR $ \r -> do
        n <- Gen.integral (Range.linearFrom 0 (-10) 10)
        pure (Fix (OpShift (Delta (fromInteger n)) r))
    , Gen.subtermM genR $ \r -> do
        n <- Gen.integral (Range.linear 1 3)
        pure (Fix (OpRepeat (fromInteger n) r))
    , Gen.subtermM genR $ \r -> do
        a <- fmap fromInteger (Gen.integral (Range.linearFrom 0 (-10) 10))
        b <- fmap fromInteger (Gen.integral (Range.linearFrom 0 (-10) 10))
        let arc = Arc (min a b) (max a b)
        pure (Fix (OpSlice arc r))
    , genSeqSubterm genR (Fix . OpConcat)
    , genSeqSubterm genR (Fix . OpMerge)
    ]

genSeqSubterm :: Gen (Op n) -> (Seq.Seq (Op n) -> Op n) -> Gen (Op n)
genSeqSubterm genR f = do
  n <- Gen.int (Range.linear 1 3)
  case n of
    1 -> Gen.subterm genR (f . Seq.singleton)
    2 -> Gen.subterm2 genR genR (\r1 r2 -> f (Seq.fromList [r1, r2]))
    3 -> Gen.subterm3 genR genR genR (\r1 r2 r3 -> f (Seq.fromList [r1, r2, r3]))
    _ -> error "impossible"

genValidOpMap :: (Ord n) => Set.Set n -> Gen (Map.Map n (Op n))
genValidOpMap validKeys = do
  let keys = Set.toList validKeys
  ops <- for [1 .. length keys] (\i -> genOp (drop i keys))
  pure (Map.fromList (zip keys ops))

-- From monad-extras
unfoldM :: (Monad m) => (s -> m (Maybe (a, s))) -> s -> m [a]
unfoldM f s = do
  mres <- f s
  case mres of
    Nothing -> return []
    Just (a, s') -> fmap (a :) (unfoldM f s')
