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
  , testCase "Tests calcLive on exercise 10.1 in book" testCalcLive2
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

testCalcLive2 :: IO ()
testCalcLive2 = uncurry calcLive (instr2graph instrs) @?= (inMap, outMap)
 where
  [m, v, n, r, s, x] = [0..5]
  instrs =
    [ OperInstr "" [] [Temp m] Nothing
    , OperInstr "" [] [Temp v] Nothing
    , LabelInstr "" (Symbol ("", 3))
    , OperInstr "" [Temp v, Temp n] [] (Just [Symbol ("", 15)])
    , MoveInstr "" (Temp v) (Temp r)
    , OperInstr "" [] [Temp s] Nothing
    , LabelInstr "" (Symbol ("", 6))
    , OperInstr "" [Temp r, Temp n] [] (Just [Symbol ("", 9)])
    , OperInstr "" [Temp v] [Temp v] Nothing
    , OperInstr "" [] [] (Just [Symbol ("", 3)])
    , LabelInstr "" (Symbol ("", 9))
    , OperInstr "" [Temp r] [Temp x] Nothing
    , OperInstr "" [Temp s, Temp x] [Temp s] Nothing
    , OperInstr "" [Temp s, Temp m] [] (Just [Symbol ("", 13)])
    , MoveInstr "" (Temp s) (Temp m)
    , LabelInstr "" (Symbol ("", 13))
    , OperInstr "" [Temp r] [Temp r] Nothing
    , OperInstr "" [] [] (Just [Symbol ("", 6)])
    , LabelInstr "" (Symbol ("", 15))
    , OperInstr "" [Temp m] [] Nothing
    ]
  inMap = IM.fromList
    [ (0, IS.fromList [n])
    , (1, IS.fromList [m,n])
    , (2, IS.fromList [m,v,n])
    , (3, IS.fromList [m,v,n])
    , (4, IS.fromList [m,v,n,r])
    , (5, IS.fromList [m,v,n,r,s])
    , (6, IS.fromList [m,v,n,r,s])
    , (7, IS.fromList [m,v,n,r,s])
    , (8, IS.fromList [m,v,n,r,s])
    , (9, IS.fromList [m,v,n,r,s,x])
    , (10, IS.fromList [m,v,n,r,s])
    , (11, IS.fromList [v,n,r,s])
    , (12, IS.fromList [m,v,n,r,s])
    , (13, IS.fromList [m,v,n,r,s])
    , (14, IS.fromList [m])
    ]
  outMap = IM.fromList
    [ (0, IS.fromList [m,n])
    , (1, IS.fromList [m,v,n])
    , (2, IS.fromList [m,v,n])
    , (3, IS.fromList [m,v,n,r])
    , (4, IS.fromList [m,v,n,r,s])
    , (5, IS.fromList [m,v,n,r,s])
    , (6, IS.fromList [m,v,n,r,s])
    , (7, IS.fromList [m,v,n,r,s])
    , (8, IS.fromList [m,v,n,r,s,x])
    , (9, IS.fromList [m,v,n,r,s])
    , (10, IS.fromList [m,v,n,r,s])
    , (11, IS.fromList [m,v,n,r,s])
    , (12, IS.fromList [m,v,n,r,s])
    , (13, IS.fromList [m,v,n,r,s])
    , (14, IS.fromList [])
    ]