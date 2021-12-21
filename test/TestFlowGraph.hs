{-# LANGUAGE TupleSections #-}
module TestFlowGraph (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Tiger.Codegen
import Tiger.Liveness (instr2graph, FlowGraph (FlowGraph))
import Tiger.Symbol (Symbol (Symbol))
import Tiger.Temp (Temp (Temp))
import qualified Data.Graph.Inductive as G
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS

tests :: TestTree
tests = testGroup "Tests instr2graph"
  [ testCase "Tests label loop" testLabelLoop
  , testCase "Tests move instruction" testMove
  , testCase "Test multiple labels" testMultipleLabels
  ]

testLabelLoop :: IO ()
testLabelLoop = instr2graph instrs @?= (graph, [0])
 where
  instrs =
    [ LabelInstr "" (Symbol ("", 0))
    , OperInstr "" [Temp 0, Temp 1] [Temp 2, Temp 3] (Just [Symbol ("", 0)])
    ]
  graph = FlowGraph
    (G.mkGraph [(0, "")] [(0, 0, ())])
    (IM.fromList [(0, [Temp 2, Temp 3])])
    (IM.fromList [(0, [Temp 0, Temp 1])])
    IS.empty

testMove :: IO ()
testMove = show (instr2graph instrs) @?= show (graph, [0, 1])
 where
  instrs = [MoveInstr "a" (Temp 0) (Temp 1), MoveInstr "b" (Temp 2) (Temp 3)]
  graph = FlowGraph
    (G.mkGraph [(0, "a"), (1, "b")] [])
    (IM.fromList [(0, [Temp 1]), (1, [Temp 3])])
    (IM.fromList [(0, [Temp 0]), (1, [Temp 2])])
    (IS.fromList [0, 1])

-- TODO: Handle jumping forward to label
testMultipleLabels :: IO ()
testMultipleLabels = instr2graph instrs @?= (graph, [0..3])
 where
  instrs =
    [ OperInstr "" [] [] Nothing
    , LabelInstr "a" (Symbol ("a", 0))
    , OperInstr "a" [] [] (Just [Symbol ("a", 0), Symbol ("c", 2)])
    , LabelInstr "b" (Symbol ("b", 1))
    , OperInstr "b" [] [] (Just [Symbol ("a", 0), Symbol ("b", 1)])
    , LabelInstr "c" (Symbol ("c", 2))
    , OperInstr "c" [] []
      (Just [Symbol ("a", 0), Symbol ("b", 1), Symbol ("c", 2)])
    ]
  graph = FlowGraph
    (G.mkGraph
      (zip [0..3] ["", "a", "b", "c"])
      [(1,1,()),(1,3,()),(2,1,()),(2,2,()),(3,1,()),(3,2,()),(3,3,())])
    (IM.fromList $ (, []) <$> [0..3])
    (IM.fromList $ (, []) <$> [0..3])
    IS.empty