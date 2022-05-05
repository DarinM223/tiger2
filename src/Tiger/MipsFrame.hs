{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Tiger.MipsFrame where

import Control.Monad (replicateM)
import Tiger.Assem (Instr (..))
import Tiger.Symbol (symbolId)
import Tiger.Temp (Label, Temp (Temp), Temp_ (..))
import Tiger.Tree
import qualified Data.IntMap.Strict as IM
import qualified Tiger.Frame as F
import Data.Foldable (foldlM)
import Data.Bifunctor (first)

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
  , frameLocals    :: Int
  , frameFormals   :: [F.Access MipsFrame]
  , frameRegisters :: MipsRegisters
  }

instance Show (F.Access MipsFrame) => Show MipsFrame where
  show = F.functionName @MipsFrame . frameName

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
  functionName lab = if s == "main" then s else s ++ show (symbolId lab)
   where s = show lab

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

frame_ :: Monad m => Temp_ m -> MipsRegisters -> F.Frame_ MipsFrame m
frame_ Temp_{..} regs =
  let
    newFrame name escapes =
      -- Because the previous $fp is saved on the local data section of
      -- the new frame ($fp - 4) in the prologue and epilogue (procEntryExit3),
      -- the locals count starts at 1 instead of 0.
      --
      -- This is what the frame data looks like:
      --
      -- [|  formal argument #2  |]
      -- -------------------------- $fp + 4
      -- [|  formal argument #1  |]
      -- -------------------------- $fp
      -- [|  saved previous $fp  |]
      -- -------------------------- $fp - 4
      -- [|  local variable #1   |]
      -- -------------------------- $fp - 8
      -- [|  local variable #2   |]
      -- -------------------------- $fp - 12
      MipsFrame name 1 <$> go escapes 0 <*> pure regs
     where
      go [] _ = pure []
      go (True:es) offset =
        (InFrame offset :) <$> go es (offset + F.wordSize @MipsFrame)
      go (False:es) offset = newTemp >>= \t ->
        (InReg t :) <$> go es (offset + F.wordSize @MipsFrame)
    allocLocal frame True = pure (InFrame offset, frame')
     where
      offset = (frameLocals frame + 1) * (-F.wordSize @MipsFrame)
      frame' = frame { frameLocals = frameLocals frame + 1 }
    allocLocal frame False = (\t -> (InReg t, frame)) <$> newTemp
    externalCall s args =
      (\label -> CallExp (NameExp label) args) <$> namedLabel s
    procEntryExit1 frame body = do
      (accs, frame') <- foldlM (const . uncurry build) ([], frame) saveRegs
      let
        args     = fmap (uncurry save) (zip (frameFormals frame) (argRegs frame))
        saves    = fmap (uncurry save) (zip accs saveRegs)
        restores = fmap (uncurry restore) (reverse (zip accs saveRegs))
      pure (stmSeq $ args ++ moveArgsToTemps ++ saves ++ [body] ++ restores, frame')
     where
      build accs frame' = first (: accs) <$> allocLocal frame' False
      initOffset = length (argRegs frame) * F.wordSize @MipsFrame
      offsets = [initOffset, initOffset + (F.wordSize @MipsFrame)..]
      -- Move spilled arguments (> 4) that are in frame into registers
      -- if the frame expects the argument to be in a register.
      moveArgsToTemps =
        go $ zip (drop (length (argRegs frame)) (frameFormals frame)) offsets
       where
        go ((InReg t, offset):fs) = restore (InFrame offset) t:go fs
        go ((InFrame _, _):fs)    = go fs -- These offsets should be the same.
        go []                     = []
      -- In page 261 of book: save and restore callee-save registers
      -- *including* the return address register.
      saveRegs = F.ra frame:calleeSaves frame
      save access reg =
        MoveStm (F.exp access (TempExp (F.fp frame))) (TempExp reg)
      restore access reg =
        MoveStm (TempExp reg) (F.exp access (TempExp (F.fp frame)))
    procEntryExit3 frame body maxFormals = pure ("", body', "")
     where
      body' =
        [ LabelInstr (show frame ++ ":") (frameName frame)
        , OperInstr "sw `s1, -4(`s0)" [F.sp frame, F.fp frame] [] Nothing
        , MoveInstr "move `d0, `s0" (F.sp frame) (F.fp frame)
        , OperInstr ("addi `d0, `s0, -" ++ show size)
          [F.sp frame] [F.sp frame] Nothing
        ] ++ body ++
        [ MoveInstr "move `d0, `s0" (F.fp frame) (F.sp frame)
        , OperInstr "lw `d0, -4(`s0)" [F.sp frame] [F.fp frame] Nothing
        , OperInstr "jr `s0" [F.ra frame] [] Nothing
        ]
      size = (frameLocals frame + max maxFormals (length (argRegs frame)))
           * F.wordSize @MipsFrame
  in F.Frame_{..}