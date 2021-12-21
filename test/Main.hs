module Main (main) where

import Test.Tasty
import qualified TestFlowGraph

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ TestFlowGraph.tests
  ]
