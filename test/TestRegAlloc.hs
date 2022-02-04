module TestRegAlloc where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Tiger.Instr (Instr (LabelInstr, OperInstr))
import Tiger.Liveness (instr2graph)
import Tiger.RegAlloc (spillCost)
import Tiger.Symbol (Symbol (Symbol))
import Tiger.Temp (Temp (Temp))

tests :: TestTree
tests = testGroup "Register allocation tests"
  [ testCase "Tests spillCost" testSpillCost
  ]

testSpillCost :: IO ()
testSpillCost = do
  cost a 4 @?= 0.5
  cost b 4 @?= 2.75
  cost c 6 @?= 0.3333333333333333
  cost d 4 @?= 5.50
  cost e 3 @?= 10.333333333333334
 where
  [a, b, c, d, e, r1, r2, r3] = Temp <$> [0..7]
  (enter, loop) = (Symbol ("", 0), Symbol ("", 1))
  cost = spillCost (fst (instr2graph instrs))
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