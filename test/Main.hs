module Main (main) where

import Test.Tasty
import qualified TestFlowGraph
import qualified TestLiveness
import qualified TestRegAlloc

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ TestFlowGraph.tests
  , TestLiveness.tests
  , TestRegAlloc.tests
  ]
