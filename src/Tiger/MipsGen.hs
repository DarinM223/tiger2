{-# LANGUAGE TypeApplications #-}
module Tiger.MipsGen (codegen) where

import Prelude hiding (exp)
import Control.Applicative (liftA2, liftA3)
import Control.Monad (void)
import Control.Monad.ST.Strict (runST)
import Data.STRef (newSTRef, readSTRef, modifySTRef')
import Tiger.Assem (Instr (..))
import Tiger.MipsFrame (MipsFrame, argRegs, callerSaves)
import Tiger.Temp (Supply (..), Temp)
import Tiger.Tree (BinOp (..), RelOp (..), Exp (..), Stm (..))
import qualified Tiger.Frame as F

-- In MIPS the argument registers are caller save so are added to calldefs.
calldefs :: MipsFrame -> [Temp]
calldefs f = F.ra f:F.rv f:callerSaves f ++ argRegs f

codegen :: Supply Temp -> MipsFrame -> Stm -> [Instr]
codegen s0 frame stm0 = runST $ do
  instrs <- newSTRef []
  let
    emit i = modifySTRef' instrs (i :)

    munchStm (S _ l r) (SeqStm a b) = munchStm l a >> munchStm r b
    munchStm (S _ l r) (MoveStm (MemExp (BinOpExp Plus e1 (ConstExp i))) e2) =
      emit =<< liftA3 (OperInstr ("sw `s1, " ++ show i ++ "(`s0)"))
        (sequence [munchExp l e1, munchExp r e2]) (pure []) (pure Nothing)
    munchStm s (MoveStm (MemExp (BinOpExp Plus (ConstExp i) e1)) e2) =
      munchStm s (MoveStm (MemExp (BinOpExp Plus e1 (ConstExp i))) e2)
    munchStm s (MoveStm (MemExp (ConstExp i)) e2) = emit =<< liftA3
      (OperInstr ("sw `s0, " ++ show i ++ "($zero)"))
      (sequence [munchExp s e2]) (pure []) (pure Nothing)
    munchStm (S _ l r) (MoveStm (MemExp e1) e2) = emit =<< liftA3
      (OperInstr "sw `s1, 0(`s0)")
      (sequence [munchExp l e1, munchExp r e2]) (pure []) (pure Nothing)
    munchStm _ (MoveStm (TempExp r) (ConstExp i)) =
      emit $ OperInstr ("li `d0, " ++ show i) [] [r] Nothing
    munchStm s (MoveStm (TempExp r) (MemExp (BinOpExp Plus e1 (ConstExp i)))) =
      emit =<< liftA3 (OperInstr ("lw `d0, " ++ show i ++ "(`s0)"))
        (sequence [munchExp s e1]) (pure [r]) (pure Nothing)
    munchStm s (MoveStm (TempExp r) (MemExp (BinOpExp Plus (ConstExp i) e1))) =
      munchStm s (MoveStm (TempExp r) (MemExp (BinOpExp Plus e1 (ConstExp i))))
    munchStm s (MoveStm (TempExp r) e2) = emit =<< liftA2
      (MoveInstr "move `d0, `s0") (munchExp s e2) (pure r)
    munchStm _ stm@(MoveStm _ _) =
      error $ "munchStm: unknown move statement " ++ show stm
    munchStm _ (JumpStm (NameExp lab) _) =
      emit $ OperInstr "j `j0" [] [] (Just [lab])
    munchStm s (JumpStm e labs) = emit =<< liftA3
      (OperInstr "jr `s0")
      (sequence [munchExp s e]) (pure []) (pure (Just labs))
    munchStm (S _ l r) (CJumpStm Eq e1 e2 t f) = emit =<< liftA3
      (OperInstr "beq `s0, `s1, `j0")
      (sequence [munchExp l e1, munchExp r e2]) (pure []) (pure (Just [t, f]))
    munchStm (S _ l r) (CJumpStm Ne e1 e2 t f) = emit =<< liftA3
      (OperInstr "bne `s0, `s1, `j0")
      (sequence [munchExp l e1, munchExp r e2]) (pure []) (pure (Just [t, f]))
    munchStm s (CJumpStm Ge e1 (ConstExp 0) t f) = emit =<< liftA3
      (OperInstr "bgez `s0, `j0")
      (sequence [munchExp s e1]) (pure []) (pure (Just [t, f]))
    munchStm (S _ l r) (CJumpStm Ge e1 e2 t f) = emit =<< liftA3
      (OperInstr "bge `s0, `s1, `j0")
      (sequence [munchExp l e1, munchExp r e2]) (pure []) (pure (Just [t, f]))
    munchStm s (CJumpStm Gt e1 (ConstExp 0) t f) = emit =<< liftA3
      (OperInstr "bgtz `s0, `j0")
      (sequence [munchExp s e1]) (pure []) (pure (Just [t, f]))
    munchStm (S _ l r) (CJumpStm Gt e1 e2 t f) = emit =<< liftA3
      (OperInstr "bgt `s0, `s1, `j0")
      (sequence [munchExp l e1, munchExp r e2]) (pure []) (pure (Just [t, f]))
    munchStm s (CJumpStm Le e1 (ConstExp 0) t f) = emit =<< liftA3
      (OperInstr "blez `s0, `j0")
      (sequence [munchExp s e1]) (pure []) (pure (Just [t, f]))
    munchStm (S _ l r) (CJumpStm Le e1 e2 t f) = emit =<< liftA3
      (OperInstr "ble `s0, `s1, `j0")
      (sequence [munchExp l e1, munchExp r e2]) (pure []) (pure (Just [t, f]))
    munchStm (S _ l r) (CJumpStm Lt e1 e2 t f) = emit =<< liftA3
      (OperInstr "blt `s0, `s1, `j0")
      (sequence [munchExp l e1, munchExp r e2]) (pure []) (pure (Just [t, f]))
    munchStm (S _ l r) (CJumpStm Ult e1 e2 t f) = emit =<< liftA3
      (OperInstr "bltu `s0, `s1, `j0")
      (sequence [munchExp l e1, munchExp r e2]) (pure []) (pure (Just [t, f]))
    munchStm (S _ l r) (CJumpStm Ule e1 e2 t f) = emit =<< liftA3
      (OperInstr "bleu `s0, `s1, `j0")
      (sequence [munchExp l e1, munchExp r e2]) (pure []) (pure (Just [t, f]))
    munchStm (S _ l r) (CJumpStm Ugt e1 e2 t f) = emit =<< liftA3
      (OperInstr "bgtu `s0, `s1, `j0")
      (sequence [munchExp l e1, munchExp r e2]) (pure []) (pure (Just [t, f]))
    munchStm (S _ l r) (CJumpStm Uge e1 e2 t f) = emit =<< liftA3
      (OperInstr "bgeu `s0, `s1, `j0")
      (sequence [munchExp l e1, munchExp r e2]) (pure []) (pure (Just [t, f]))
    munchStm (S _ l r) (ExpStm (CallExp e args)) = emit =<< liftA3
      (OperInstr "jalr `s0")
      (liftA2 (:) (munchExp l e) (munchArgs r 0 initOffset args))
      (pure (calldefs frame)) (pure Nothing)
     where initOffset = (length (argRegs frame) + 1) * F.wordSize @MipsFrame
    munchStm s (ExpStm e) = void $ munchExp s e
    munchStm _ (LabelStm lab) = emit $ LabelInstr (show lab ++ ":") lab

    munchArgs (S _ (S _ s1 s2) s3) n off (e:es) = do
      src <- munchExp s1 e
      let args = argRegs frame
      if n < length args
        then do
          let dst = args !! n
          munchStm s2 $ MoveStm (TempExp dst) (TempExp src)
          (dst :) <$> munchArgs s3 (n + 1) off es
        else do
          -- Remember that the caller can put something at a certain
          -- offset from $sp and the callee will see it from the same
          -- offset from $fp (book pg 136). So we are using $sp to store
          -- arguments into the parameter locations of the callee's frame.
          let storeLoc = MemExp (BinOpExp Plus (TempExp (F.sp frame)) (ConstExp off))
          munchStm s2 $ MoveStm storeLoc (TempExp src)
          munchArgs s3 (n + 1) (off + F.wordSize @MipsFrame) es
    munchArgs _ _ _ [] = pure []

    munchExp (S r _ s) (MemExp (BinOpExp Plus e1 (ConstExp i))) = (r <$) $
      emit =<< liftA3 (OperInstr ("lw `d0, " ++ show i ++ "(`s0)"))
        (sequence [munchExp s e1]) (pure [r]) (pure Nothing)
    munchExp s (MemExp (BinOpExp Plus (ConstExp i) e1)) =
      munchExp s (MemExp (BinOpExp Plus e1 (ConstExp i)))
    munchExp (S r _ _) (MemExp (ConstExp i)) = (r <$) $
      emit $ OperInstr ("lw `d0, " ++ show i ++ "($zero)") [] [r] Nothing
    munchExp (S r _ s) (MemExp e) = (r <$) $
      emit =<< liftA3 (OperInstr "lw `d0, 0(`s0)")
        (sequence [munchExp s e]) (pure [r]) (pure Nothing)

    munchExp (S r _ s) (BinOpExp Plus e1 (ConstExp i)) = (r <$) $
      emit =<< liftA3 (OperInstr ("addi `d0, `s0, " ++ show i))
        (sequence [munchExp s e1]) (pure [r]) (pure Nothing)
    munchExp s (BinOpExp Plus (ConstExp i) e1) =
      munchExp s (BinOpExp Plus e1 (ConstExp i))
    munchExp (S r s1 s2) (BinOpExp Plus e1 e2) = (r <$) $
      emit =<< liftA3 (OperInstr "add `d0, `s0, `s1")
        (sequence [munchExp s1 e1, munchExp s2 e2]) (pure [r]) (pure Nothing)

    munchExp s (BinOpExp Minus e1 (ConstExp i)) =
      munchExp s (BinOpExp Plus e1 (ConstExp (-i)))
    munchExp (S r s1 s2) (BinOpExp Minus e1 e2) = (r <$) $
      emit =<< liftA3 (OperInstr "sub `d0, `s0, `s1")
        (sequence [munchExp s1 e1, munchExp s2 e2]) (pure [r]) (pure Nothing)

    munchExp (S r s1 s2) (BinOpExp Mul e1 e2) = (r <$) $
      emit =<< liftA3 (OperInstr "mul `d0, `s0, `s1")
        (sequence [munchExp s1 e1, munchExp s2 e2]) (pure [r]) (pure Nothing)
    munchExp (S r s1 s2) (BinOpExp Div e1 e2) = (r <$) $
      emit =<< liftA3 (OperInstr "div `d0, `s0, `s1")
        (sequence [munchExp s1 e1, munchExp s2 e2]) (pure [r]) (pure Nothing)

    munchExp (S r _ s) (BinOpExp And e1 (ConstExp i)) = (r <$) $
      emit =<< liftA3 (OperInstr ("andi `d0, `s0, " ++ show i))
        (sequence [munchExp s e1]) (pure [r]) (pure Nothing)
    munchExp s (BinOpExp And (ConstExp i) e1) =
      munchExp s (BinOpExp And e1 (ConstExp i))
    munchExp (S r s1 s2) (BinOpExp And e1 e2) = (r <$) $
      emit =<< liftA3 (OperInstr "and `d0, `s0, `s1")
        (sequence [munchExp s1 e1, munchExp s2 e2]) (pure [r]) (pure Nothing)

    munchExp (S r _ s) (BinOpExp Or e1 (ConstExp i)) = (r <$) $
      emit =<< liftA3 (OperInstr ("ori `d0, `s0, " ++ show i))
        (sequence [munchExp s e1]) (pure [r]) (pure Nothing)
    munchExp s (BinOpExp Or (ConstExp i) e1) =
      munchExp s (BinOpExp Or e1 (ConstExp i))
    munchExp (S r s1 s2) (BinOpExp Or e1 e2) = (r <$) $
      emit =<< liftA3 (OperInstr "or `d0, `s0, `s1")
        (sequence [munchExp s1 e1, munchExp s2 e2]) (pure [r]) (pure Nothing)

    munchExp (S r _ s) (BinOpExp Xor e1 (ConstExp i)) = (r <$) $
      emit =<< liftA3 (OperInstr ("xori `d0, `s0, " ++ show i))
        (sequence [munchExp s e1]) (pure [r]) (pure Nothing)
    munchExp s (BinOpExp Xor (ConstExp i) e1) =
      munchExp s (BinOpExp Xor e1 (ConstExp i))
    munchExp (S r s1 s2) (BinOpExp Xor e1 e2) = (r <$) $
      emit =<< liftA3 (OperInstr "xor `d0, `s0, `s1")
        (sequence [munchExp s1 e1, munchExp s2 e2]) (pure [r]) (pure Nothing)

    munchExp (S r _ s) (BinOpExp Lshift e1 (ConstExp i)) = (r <$) $
      emit =<< liftA3 (OperInstr ("sll `d0, `s0, " ++ show i))
        (sequence [munchExp s e1]) (pure [r]) (pure Nothing)
    munchExp (S r s1 s2) (BinOpExp Lshift e1 e2) = (r <$) $
      emit =<< liftA3 (OperInstr "sllv `d0, `s0, `s1")
        (sequence [munchExp s1 e1, munchExp s2 e2]) (pure [r]) (pure Nothing)

    munchExp (S r _ s) (BinOpExp Rshift e1 (ConstExp i)) = (r <$) $
      emit =<< liftA3 (OperInstr ("srl `d0, `s0, " ++ show i))
        (sequence [munchExp s e1]) (pure [r]) (pure Nothing)
    munchExp (S r s1 s2) (BinOpExp Rshift e1 e2) = (r <$) $
      emit =<< liftA3 (OperInstr "srlv `d0, `s0, `s1")
        (sequence [munchExp s1 e1, munchExp s2 e2]) (pure [r]) (pure Nothing)

    munchExp (S r _ s) (BinOpExp Arshift e1 (ConstExp i)) = (r <$) $
      emit =<< liftA3 (OperInstr ("sra `d0, `s0, " ++ show i))
        (sequence [munchExp s e1]) (pure [r]) (pure Nothing)
    munchExp (S r s1 s2) (BinOpExp Arshift e1 e2) = (r <$) $
      emit =<< liftA3 (OperInstr "srav `d0, `s0, `s1")
        (sequence [munchExp s1 e1, munchExp s2 e2]) (pure [r]) (pure Nothing)

    munchExp s e@(CallExp _ _) = F.rv frame <$ munchStm s (ExpStm e)
    munchExp (S r _ _) (ConstExp i) = (r <$) $
      emit $ OperInstr ("li `d0, " ++ show i) [] [r] Nothing
    munchExp _ (TempExp t) = pure t
    munchExp (S r _ _) (NameExp lab) = (r <$) $
      emit $ OperInstr ("la `d0, " ++ show lab) [] [r] Nothing
    munchExp (S _ s1 s2) (ESeqExp stm exp) = munchStm s1 stm >> munchExp s2 exp
  munchStm s0 stm0
  reverse <$> readSTRef instrs