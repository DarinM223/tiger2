{-# LANGUAGE RecordWildCards #-}
module TestRegAlloc where

import Control.Applicative (liftA2)
import Data.Bifunctor (first)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Tiger (State (..), mkState)
import Tiger.Assem (Instr (LabelInstr, OperInstr))
import Tiger.Color (color)
import Tiger.Liveness (instr2graph, interferenceGraph)
import Tiger.RegAlloc (rewrite, spillCost)
import Tiger.Symbol (Symbol (Symbol))
import Tiger.Temp (Temp (..), mkSupply, mkTempGen)
import qualified Data.IntMap.Strict as IM
import qualified Tiger.Frame as F

tests :: TestTree
tests = testGroup "Register allocation tests"
  [ testCase "Tests spillCost with program 11.8" testSpillCost
  , testCase "Tests rewrite with program 11.8" testRewrite
  , testCase "Tests alloc with program 11.8" testColor
  ]

a, b, c, d, e, r1, r2, r3 :: Temp
[a, b, c, d, e, r1, r2, r3] = Temp <$> [100..107]

enter, loop :: Symbol
(enter, loop) = (Symbol ("", 0), Symbol ("", 1))

instrs :: [Instr]
instrs =
  [ LabelInstr "" enter
  , OperInstr "" [r3] [c] Nothing
  , OperInstr "" [r1] [a] Nothing
  , OperInstr "" [r2] [b] Nothing
  , OperInstr "" [] [d] Nothing
  , OperInstr "" [a] [e] Nothing
  , LabelInstr "" loop
  , OperInstr "" [d, b] [d] Nothing
  , OperInstr "" [e] [e] Nothing
  , OperInstr "" [e] [] (Just [loop])
  , OperInstr "" [d] [r1] Nothing
  , OperInstr "" [c] [r3] Nothing
  ]

testSpillCost :: IO ()
testSpillCost = do
  cost a @?= 0.5
  cost b @?= 2.75
  cost c @?= 0.3333333333333333
  cost d @?= 5.50
  cost e @?= 10.333333333333334
 where
  (g, ns) = instr2graph instrs
  cost = spillCost g (fst (interferenceGraph g ns))

testRewrite :: IO ()
testRewrite = do
  State{frameIO = F.Frame_{..},..} <- mkState
  frame <- newFrame (Symbol ("", 3)) []
  localSupply <- mkSupply (allocLocal frame True)
  let instrs'  = rewrite (liftA2 (,) tempSupply localSupply) instrs frame [c]
      expected =
        [ LabelInstr "" enter
        , OperInstr "" [r3] [Temp 25] Nothing
        , OperInstr "sw `s1, -8(`s0)" [F.fp frame, Temp 25] [] Nothing
        , OperInstr "" [r1] [a] Nothing
        , OperInstr "" [r2] [b] Nothing
        , OperInstr "" [] [d] Nothing
        , OperInstr "" [a] [e] Nothing
        , LabelInstr "" loop
        , OperInstr "" [d, b] [d] Nothing
        , OperInstr "" [e] [e] Nothing
        , OperInstr "" [e] [] (Just [loop])
        , OperInstr "" [d] [r1] Nothing
        , OperInstr "lw `d0, -8(`s0)" [F.fp frame] [Temp 26] Nothing
        , OperInstr "" [Temp 26] [r3] Nothing
        ]
  instrs' @?= expected

testColor :: IO ()
testColor = do
  let
    (g, ns) = instr2graph instrs
    (ig, _) = interferenceGraph g ns
    initial = IM.fromList $ fmap (first unTemp) [(r1, "r1"), (r2, "r2"), (r3, "r3")]
    (_, spills) = color ig initial (spillCost g ig) ["r1", "r2", "r3"]
  spills @?= [c]
  let
    (c1, c2) = (Temp 34, Temp 35)
    rewritten =
      [ LabelInstr "" enter
      , OperInstr "" [r3] [c1] Nothing
      , OperInstr "sw `s1, -8(`s0)" [c1] [] Nothing
      , OperInstr "" [r1] [a] Nothing
      , OperInstr "" [r2] [b] Nothing
      , OperInstr "" [] [d] Nothing
      , OperInstr "" [a] [e] Nothing
      , LabelInstr "" loop
      , OperInstr "" [d, b] [d] Nothing
      , OperInstr "" [e] [e] Nothing
      , OperInstr "" [e] [] (Just [loop])
      , OperInstr "" [d] [r1] Nothing
      , OperInstr "lw `d0, -8(`s0)" [] [c2] Nothing
      , OperInstr "" [c2] [r3] Nothing
      ]
    (g', ns') = instr2graph rewritten
    (ig', _) = interferenceGraph g' ns'
    (alloc', spills') = color ig' initial (spillCost g' ig') ["r1", "r2", "r3"]
    expected =
      [ (c1, "r3"), (c2, "r3"), (a, "r1"), (b, "r2"), (d, "r3"), (e, "r1")
      , (r1, "r1"), (r2, "r2"), (r3, "r3") ]
  spills' @?= []
  alloc' @?= IM.fromList (fmap (first unTemp) expected)