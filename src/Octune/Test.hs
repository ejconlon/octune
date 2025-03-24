module Octune.Test (main) where

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Test Suite"
    [ testCase "Simple test example" $
        2 + 2 @?= (4 :: Int)
    ]
