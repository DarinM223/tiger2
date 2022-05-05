{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Tiger.Frame where

import Tiger.Assem (Instr)
import Tiger.Temp (Label, Temp)
import qualified Data.IntMap.Strict as IM
import qualified Tiger.Tree as Tree

type Register = String

class Frame frame where
  data Access frame
  name           :: frame -> Label
  formals        :: frame -> [Access frame]
  fp             :: frame -> Temp
  sp             :: frame -> Temp
  rv             :: frame -> Temp
  ra             :: frame -> Temp
  tempMap        :: frame -> IM.IntMap Register
  wordSize       :: Int
  exp            :: Access frame -> Tree.Exp -> Tree.Exp
  string         :: Label -> String -> String
  procEntryExit2 :: frame -> [Instr] -> [Instr]
  functionName   :: Label -> String

data Frame_ frame m = Frame_
  { newFrame       :: Label -> [Bool] -> m frame
  , allocLocal     :: frame -> Bool -> m (Access frame, frame)
  , externalCall   :: String -> [Tree.Exp] -> m Tree.Exp
  , procEntryExit1 :: frame -> Tree.Stm -> m (Tree.Stm, frame)
  , procEntryExit3 :: frame -> [Instr] -> Int -> m (String, [Instr], String)
  }