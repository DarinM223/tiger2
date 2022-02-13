{-# LANGUAGE TupleSections #-}
module TestFlowGraph (tests) where

import Data.Foldable (foldl')
import Test.Tasty
import Test.Tasty.HUnit
import Tiger.Assem
import Tiger.Liveness (FlowGraph (FlowGraph), insEdge, instr2graph)
import Tiger.Symbol (Symbol (Symbol))
import Tiger.Temp (Temp (Temp))
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS

tests :: TestTree
tests = testGroup "Tests instr2graph"
  [ testCase "Tests label loop" testLabelLoop
  , testCase "Test multiple labels" testMultipleLabels
  , testCase "Test label at end" testLabelEnd
  ]

testLabelLoop :: IO ()
testLabelLoop = instr2graph instrs @?= (graph, [0])
 where
  instrs =
    [ LabelInstr "" (Symbol ("", 0))
    , OperInstr "" [Temp 0, Temp 1] [Temp 2, Temp 3] (Just [Symbol ("", 0)])
    ]
  graph = FlowGraph
    (IM.fromList [(0, IS.fromList [0])])
    (IM.fromList [(0, IS.fromList [2, 3])])
    (IM.fromList [(0, IS.fromList [0, 1])])
    IS.empty

testMultipleLabels :: IO ()
testMultipleLabels = instr2graph instrs @?= (graph, [3,2..0])
 where
  instrs =
    [ OperInstr "" [] [] Nothing
    , LabelInstr "a" (Symbol ("a", 0))
    , OperInstr "a" [] [] (Just [Symbol ("a", 0), Symbol ("b", 1), Symbol ("c", 2)])
    , LabelInstr "b" (Symbol ("b", 1))
    , OperInstr "b" [] [] (Just [Symbol ("a", 0), Symbol ("b", 1), Symbol ("c", 2)])
    , LabelInstr "c" (Symbol ("c", 2))
    , OperInstr "c" [] []
      (Just [Symbol ("a", 0), Symbol ("b", 1), Symbol ("c", 2)])
    ]
  edges = (0, 1):[(a, b) | a <- [1..3], b <- [1..3]]
  graph = FlowGraph
    (foldl' (flip (uncurry insEdge)) IM.empty edges)
    (IM.fromList $ (, IS.empty) <$> [0..3])
    (IM.fromList $ (, IS.empty) <$> [0..3])
    IS.empty

testLabelEnd :: IO ()
testLabelEnd = do
  instr2graph instrs1 @?= (expected1, [0])
  instr2graph instrs2 @?= (expected2, [2, 1, 0])
 where
  instrs1 = [LabelInstr "" (Symbol ("", 0))]
  expected1 = FlowGraph
    (IM.fromList [(0, IS.fromList [])])
    (IM.fromList [(0, IS.fromList [])])
    (IM.fromList [(0, IS.fromList [])])
    (IS.fromList [])
  instrs2 =
    [ OperInstr "" [Temp 1] [Temp 2] (Just [Symbol ("", 0), Symbol ("", 1)])
    , LabelInstr "" (Symbol ("", 1))
    , OperInstr "" [Temp 3] [Temp 4] Nothing
    , LabelInstr "" (Symbol ("", 0))
    ]
  expected2 = FlowGraph
    (IM.fromList [(0, IS.fromList [1, 2]), (1, IS.fromList [2]),(2, IS.fromList [])])
    (IM.fromList [(0, IS.fromList [2]), (1, IS.fromList [4]), (2, IS.fromList [])])
    (IM.fromList [(0, IS.fromList [1]), (1, IS.fromList [3]), (2, IS.fromList [])])
    (IS.fromList [])