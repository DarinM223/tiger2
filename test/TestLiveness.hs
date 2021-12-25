module TestLiveness (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Tiger.Codegen (Instr (..))
import Tiger.Liveness (IGraph (..), calcLive, instr2graph, interferenceGraph)
import Tiger.Symbol (Symbol (Symbol))
import Tiger.Temp (Temp (Temp))
import qualified Data.Graph.Inductive as G
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS

tests :: TestTree
tests = testGroup "Liveness tests"
  [ testCase "Tests calcLive on graph 10.1 in book" testCalcLive
  ]

gr :: (IGraph, b) -> G.Gr Temp ()
gr = iGraph . fst

testCalcLive :: IO ()
testCalcLive = uncurry calcLive (instr2graph instrs) @?= (inMap, outMap)
 where
  -- Temporaries: a -> 0, b -> 1, c -> 2
  instrs =
    [ OperInstr "" [] [Temp 0] Nothing
    , LabelInstr "" (Symbol ("", 0))
    , OperInstr "" [Temp 0] [Temp 1] Nothing
    , OperInstr "" [Temp 1, Temp 2] [Temp 2] Nothing
    , OperInstr "" [Temp 1] [Temp 0] Nothing
    , OperInstr "" [Temp 0] [] (Just [Symbol ("", 0)])
    , OperInstr "" [Temp 2] [] Nothing
    ]
  -- Results are in tables 10.5 and 10.6 in book.
  inMap = IM.fromList
    [ (0, IS.fromList [2])
    , (1, IS.fromList [0, 2])
    , (2, IS.fromList [1, 2])
    , (3, IS.fromList [1, 2])
    , (4, IS.fromList [0, 2])
    , (5, IS.fromList [2])
    ]
  outMap = IM.fromList
    [ (0, IS.fromList [0, 2])
    , (1, IS.fromList [1, 2])
    , (2, IS.fromList [1, 2])
    , (3, IS.fromList [0, 2])
    , (4, IS.fromList [0, 2])
    , (5, IS.empty)
    ]