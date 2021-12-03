{-# LANGUAGE FlexibleContexts #-}
module Tiger.MipsGen where

import Control.Applicative (liftA3)
import Tiger.Codegen
import Tiger.Temp (MonadTemp (newTemp), Temp)
import Tiger.Translate (MonadPut (put))
import Tiger.Tree

munchStm :: (MonadTemp m, MonadPut Instr m) => Stm -> m ()
munchStm (SeqStm a b) = munchStm a >> munchStm b
munchStm (MoveStm (MemExp (BinOpExp Plus e1 (ConstExp i))) e2) = put =<< liftA3
  (OperInstr ("sw `s1, " ++ show i ++ "(`s0)"))
  (traverse munchExp [e1, e2]) (pure []) (pure Nothing)
munchStm (MoveStm (MemExp (BinOpExp Plus (ConstExp i) e1)) e2) = put =<< liftA3
  (OperInstr ("sw `s1, " ++ show i ++ "(`s0)"))
  (traverse munchExp [e1, e2]) (pure []) (pure Nothing)
munchStm (MoveStm (MemExp (ConstExp i)) e2) = put =<< liftA3
  (OperInstr ("sw `s0, " ++ show i ++ "($zero)"))
  (traverse munchExp [e2]) (pure []) (pure Nothing)
munchStm (MoveStm (MemExp e1) e2) = put =<< liftA3
  (OperInstr "sw `s1, 0(`s0)")
  (traverse munchExp [e1, e2]) (pure []) (pure Nothing)
munchStm (MoveStm (TempExp i) e2) = put =<< liftA3
  (OperInstr "move `d0, `s0")
  (traverse munchExp [e2]) (pure [i]) (pure Nothing)
munchStm (LabelStm lab) = put $ OperInstr (show lab ++ ":") [] [] Nothing
munchStm s = error $ "munchStm: unknown statement " ++ show s

munchExp :: (MonadTemp m, MonadPut Instr m) => Exp -> m Temp
munchExp (MemExp (BinOpExp Plus e1 (ConstExp i))) = result $ \r ->
  put =<< liftA3 (OperInstr ("lw `d0, " ++ show i ++ "(`s0)"))
    (traverse munchExp [e1]) (pure [r]) (pure Nothing)
munchExp (MemExp (BinOpExp Plus (ConstExp i) e1)) = result $ \r ->
  put =<< liftA3 (OperInstr ("lw `d0, " ++ show i ++ "(`s0)"))
    (traverse munchExp [e1]) (pure [r]) (pure Nothing)
munchExp (MemExp (ConstExp i)) = result $ \r ->
  put $ OperInstr ("lw `d0, " ++ show i ++ "($zero)") [] [r] Nothing
munchExp (MemExp e) = result $ \r ->
  put =<< liftA3 (OperInstr "lw `d0, 0(`s0)")
    (traverse munchExp [e]) (pure [r]) (pure Nothing)
munchExp (BinOpExp Plus e1 (ConstExp i)) = result $ \r ->
  put =<< liftA3 (OperInstr ("addi `d0, `s0, " ++ show i))
    (traverse munchExp [e1]) (pure [r]) (pure Nothing)
munchExp (BinOpExp Plus (ConstExp i) e1) = result $ \r ->
  put =<< liftA3 (OperInstr ("addi `d0, `s0, " ++ show i))
    (traverse munchExp [e1]) (pure [r]) (pure Nothing)
munchExp (BinOpExp Plus e1 e2) = result $ \r ->
  put =<< liftA3 (OperInstr "add `d0, `s0, `s1")
    (traverse munchExp [e1, e2]) (pure [r]) (pure Nothing)
munchExp (ConstExp i) = result $ \r ->
  put $ OperInstr ("li `d0, " ++ show i) [] [r] Nothing
munchExp (TempExp t) = pure t
munchExp e = error $ "munchExp: unknown expression " ++ show e

result :: MonadTemp m => (Temp -> m a) -> m Temp
result f = newTemp >>= \t -> t <$ f t