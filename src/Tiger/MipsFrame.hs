{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Tiger.MipsFrame where

import Control.Monad (replicateM)
import Tiger.Assem (Instr (..))
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
  tempMap = regTempMap . frameRegisters
  wordSize = 4
  exp (InFrame k) temp = MemExp $ BinOpExp Plus temp (ConstExp k)
  exp (InReg t) _ = TempExp t
  string l s = show l ++ ": .asciiz \"" ++ s ++ "\""
  procEntryExit2 frame body = body ++ [OperInstr "" src [] (Just [])]
   where
    src = [regZero $ frameRegisters frame, F.ra frame, F.sp frame]
       ++ calleeSaves frame

specialRegs :: F.Frame frame => frame -> [Temp]
specialRegs f = ($ f) <$> [F.fp, F.sp, F.rv, F.ra]

argRegs :: MipsFrame -> [Temp]
argRegs = regArgRegs . frameRegisters

-- | Registers that the callee can trash.
callerSaves :: MipsFrame -> [Temp]
callerSaves = regCallerSaves . frameRegisters

-- | Registers that the callee must preserve unchanged.
calleeSaves :: MipsFrame -> [Temp]
calleeSaves = regCalleeSaves . frameRegisters

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
    procEntryExit1 frame body = do
      accs <- traverse (\_ -> allocLocal frame False) saveRegs
      let
        args     = fmap (uncurry save) (zip (frameFormals frame) (argRegs frame))
        saves    = fmap (uncurry save) (zip accs saveRegs)
        restores = fmap (uncurry restore) (reverse (zip accs saveRegs))
      pure $ stmSeq $ args ++ saves ++ [body] ++ restores
     where
      -- In page 261 of book: save and restore callee-save registers
      -- *including* the return address register.
      saveRegs = F.ra frame:calleeSaves frame
      save access reg =
        MoveStm (F.exp access (TempExp (F.fp frame))) (TempExp reg)
      restore access reg =
        MoveStm (TempExp reg) (F.exp access (TempExp (F.fp frame)))
    procEntryExit3 frame body = do
      locals <- readIntVar $ frameLocals frame
      let
        size = (locals + length (argRegs frame)) * F.wordSize @MipsFrame
        body' =
          [ LabelInstr (show (frameName frame) ++ ":") (frameName frame)
          , OperInstr "sw `s1, 0(`s0)" [F.sp frame, F.fp frame] [] Nothing
          , MoveInstr "move `d0, `s0" (F.sp frame) (F.fp frame)
          , OperInstr ("addi `d0, `s0, -" ++ show size)
            [F.sp frame] [F.sp frame] Nothing
          ] ++ body ++
          [ MoveInstr "move `d0, `s0" (F.fp frame) (F.sp frame)
          , OperInstr "lw `d0, 0(`s0)" [F.sp frame] [F.fp frame] Nothing
          , OperInstr "jr `s0" [F.ra frame] [] Nothing
          ]
      pure ("", body', "")
  in F.Frame_{..}