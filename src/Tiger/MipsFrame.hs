{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Tiger.MipsFrame where

import Control.Monad (replicateM)
import Tiger.Instr (Instr (OperInstr))
import Tiger.IntVar (IntVar, readIntVar, writeIntVar, newIntVar)
import Tiger.Temp (Label, Temp (Temp), Temp_ (..))
import Tiger.Tree
import qualified Data.IntMap.Strict as IM
import qualified Tiger.Frame as F

data MipsRegisters = MipsRegisters
  { regZero        :: Temp
  , regFp          :: Temp
  , regSp          :: Temp
  , regRv          :: Temp
  , regRa          :: Temp
  , regArgRegs     :: [Temp]
  , regCallerSaves :: [Temp]
  , regCalleeSaves :: [Temp]
  , regTempMap     :: IM.IntMap F.Register
  }

mkMipsRegisters :: IO Temp -> IO MipsRegisters
mkMipsRegisters temp = do
  regZero <- temp
  regFp <- temp
  regSp <- temp
  regRv <- temp
  regRa <- temp
  regArgRegs <- replicateM 4 temp
  regCallerSaves <- replicateM 8 temp
  regCalleeSaves <- replicateM 8 temp
  let special = [(regFp, "$fp"), (regRv, "$v0"), (regSp, "$sp"), (regRa, "$ra")]
      regTempMap = IM.fromList $ keys "$a" regArgRegs
                              ++ keys "$t" regCallerSaves
                              ++ keys "$s" regCalleeSaves
                              ++ fmap (\(Temp t, s) -> (t, s)) special
  return MipsRegisters{..}
 where
  keys pre = fmap (\(i, Temp t) -> (t, pre ++ show i)) . zip [(0 :: Int)..]

data MipsFrame = MipsFrame
  { frameName      :: Label
  , frameLocals    :: IntVar
  , frameFormals   :: [F.Access MipsFrame]
  , frameRegisters :: MipsRegisters
  }

instance Show (F.Access MipsFrame) => Show MipsFrame where
  show = show . frameName

instance F.Frame MipsFrame where
  data Access MipsFrame = InFrame Int | InReg Temp deriving Show
  name = frameName
  formals = frameFormals
  fp = regFp . frameRegisters
  sp = regSp . frameRegisters
  rv = regRv . frameRegisters
  ra = regRa . frameRegisters
  specialRegs f = ($ f) <$> [F.fp, F.sp, F.rv, F.ra]
  argRegs = regArgRegs . frameRegisters
  callerSaves = regCallerSaves . frameRegisters
  calleeSaves = regCalleeSaves . frameRegisters
  tempMap = regTempMap . frameRegisters
  tempName (Temp t) f = case IM.lookup t (F.tempMap f) of
    Just n  -> n
    Nothing -> "t" ++ show t
  registers = IM.elems . F.tempMap
  wordSize = 4
  exp (InFrame k) temp = MemExp $ BinOpExp Plus temp (ConstExp k)
  exp (InReg t) _ = TempExp t
  procEntryExit2 frame body = body ++ [OperInstr "" src [] (Just [])]
   where
    src = [regZero $ frameRegisters frame, F.ra frame, F.sp frame]
       ++ F.calleeSaves frame

frameIO :: Temp_ IO -> MipsRegisters -> F.Frame_ MipsFrame IO
frameIO Temp_{..} regs =
  let
    newFrame name escapes =
      MipsFrame name <$> newIntVar 0 <*> go escapes 0 <*> pure regs
     where
      go [] _ = pure []
      go (True:es) offset =
        (InFrame offset :) <$> go es (offset + F.wordSize @MipsFrame)
      go (False:es) offset = newTemp >>= \t -> (InReg t :) <$> go es offset
    allocLocal frame True = do
      locals <- readIntVar $ frameLocals frame
      let offset = (locals + 1) * F.wordSize @MipsFrame
      writeIntVar (frameLocals frame) (locals + 1)
      pure $ InFrame offset
    allocLocal _ False = InReg <$> newTemp
    externalCall s args =
      (\label -> CallExp (NameExp label) args) <$> namedLabel s
    procEntryExit1 _ body = pure body
  in F.Frame_{..}