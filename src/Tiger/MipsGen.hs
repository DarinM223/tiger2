{-# LANGUAGE FlexibleContexts #-}
module Tiger.MipsGen where
import Tiger.Translate (MonadPut (put))
import Tiger.Tree
import Tiger.Temp (MonadTemp, Temp (Temp))
import Tiger.Codegen
import Control.Applicative (liftA3)

munchStm :: (MonadTemp m, MonadPut Instr m) => Stm -> m ()
munchStm (SeqStm a b) = munchStm a >> munchStm b
munchStm (MoveStm (MemExp (BinOpExp Plus e1 (ConstExp i))) e2) = put =<< liftA3
  (OperInstr ("sw `s1, " ++ show i ++ "(`s0)"))
  (traverse munchExp [e1, e2]) (pure []) (pure Nothing)
munchStm (MoveStm (MemExp (BinOpExp Plus (ConstExp i) e1)) e2) = put =<< liftA3
  (OperInstr ("sw `s1, " ++ show i ++ "(`s0)"))
  (traverse munchExp [e1, e2]) (pure []) (pure Nothing)
munchStm (MoveStm (MemExp e1) (MemExp e2)) = put =<< liftA3
  (OperInstr "move `s0, `s1")
  (traverse munchExp [e1, e2]) (pure []) (pure Nothing)
munchStm _ = undefined

munchExp :: (MonadTemp m, MonadPut Instr m) => Exp -> m Temp
munchExp _ = undefined