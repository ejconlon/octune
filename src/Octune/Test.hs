module Octune.Test (main) where

import PropUnit (TestTree, testMain, testGroup, testUnit, (===))

main :: IO ()
main = testMain (const tests)

tests :: TestTree
tests =
  testGroup
    "Test Suite"
    [ testUnit "Simple test example" $
        2 + 2 === (4 :: Int)
    ]
