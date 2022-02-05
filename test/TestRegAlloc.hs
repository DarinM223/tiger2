{-# LANGUAGE RecordWildCards #-}
module TestRegAlloc where

import Control.Applicative (liftA2)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Tiger (State (..), mkState)
import Tiger.Instr (Instr (LabelInstr, OperInstr))
import Tiger.Liveness (instr2graph)
import Tiger.RegAlloc (rewrite, spillCost)
import Tiger.Symbol (Symbol (Symbol))
import Tiger.Temp (Temp (Temp), mkSupply, mkTempGen)
import qualified Tiger.Frame as F

tests :: TestTree
tests = testGroup "Register allocation tests"
  [ testCase "Tests spillCost with program 11.8" testSpillCost
  , testCase "Tests rewrite with program 11.8" testRewrite
  ]

a, b, c, d, e, r1, r2, r3 :: Temp
[a, b, c, d, e, r1, r2, r3] = Temp <$> [0..7]

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
  cost a 4 @?= 0.5
  cost b 4 @?= 2.75
  cost c 6 @?= 0.3333333333333333
  cost d 4 @?= 5.50
  cost e 3 @?= 10.333333333333334
 where cost = spillCost (fst (instr2graph instrs))

testRewrite :: IO ()
testRewrite = do
  State{frameIO = F.Frame_{..},..} <- mkState
  frame <- newFrame (Symbol ("", 3)) []
  localSupply <- mkSupply (allocLocal frame True)
  let instrs'  = rewrite (liftA2 (,) tempSupply localSupply) instrs frame [c]
      expected =
        [ LabelInstr "" enter
        , OperInstr "" [r3] [Temp 34] Nothing
        , OperInstr "sw `s1, 4(`s0)" [F.fp frame, Temp 34] [] Nothing
        , OperInstr "" [r1] [a] Nothing
        , OperInstr "" [r2] [b] Nothing
        , OperInstr "" [] [d] Nothing
        , OperInstr "" [a] [e] Nothing
        , LabelInstr "" loop
        , OperInstr "" [d, b] [d] Nothing
        , OperInstr "" [e] [e] Nothing
        , OperInstr "" [e] [] (Just [loop])
        , OperInstr "" [d] [r1] Nothing
        , OperInstr "lw `d0, 4(`s0)" [F.fp frame] [Temp 35] Nothing
        , OperInstr "" [Temp 35] [r3] Nothing
        ]
  instrs' @?= expected