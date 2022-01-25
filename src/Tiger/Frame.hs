{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Tiger.Frame where

import Tiger.Instr (Instr)
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
  specialRegs    :: frame -> [Temp]
  argRegs        :: frame -> [Temp]
  -- | Registers that the callee can trash.
  callerSaves    :: frame -> [Temp]
  -- | Registers that the callee must preserve unchanged.
  calleeSaves    :: frame -> [Temp]
  registers      :: frame -> [Register]
  tempMap        :: frame -> IM.IntMap Register
  tempName       :: Temp -> frame -> String
  wordSize       :: Int
  exp            :: Access frame -> Tree.Exp -> Tree.Exp
  procEntryExit2 :: frame -> [Instr] -> [Instr]

class (Monad m, Frame (Frame' m)) => MonadFrame m where
  type Frame' m
  newFrame       :: Label -> [Bool] -> m (Frame' m)
  allocLocal     :: Frame' m -> Bool -> m (Access (Frame' m))
  externalCall   :: String -> [Tree.Exp] -> m Tree.Exp
  procEntryExit1 :: Frame' m -> Tree.Stm -> m Tree.Stm