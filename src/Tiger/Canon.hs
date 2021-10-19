{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
module Tiger.Canon where

import Data.Bifunctor (first)
import Tiger.Temp (Label, Supply (S), Temp)
import Tiger.Tree

errStr :: String
errStr = "Error"

class Fn f a b where
  fn :: f -> [a] -> b

instance Fn (a -> b) a b where
  fn f (a:_) = f a
  fn _ _ = error errStr

instance Fn (a -> a -> b) a b where
  fn f (a:b:_) = f a b
  fn _ _ = error errStr

instance Fn (a -> a -> a -> b) a b where
  fn f (a:b:c:_) = f a b c
  fn _ _ = error errStr

instance Fn (a -> [a] -> b) a b where
  fn f (a:rest) = f a rest
  fn _ _ = error errStr

linearize :: Supply Temp -> Stm -> [Stm]
linearize s0 = flip linear [] . doStm s0
 where
  reorder :: Supply Temp -> [Exp] -> (Stm, [Exp])
  reorder (S !t _ s) (CallExp f args:es) =
    reorder s (ESeqExp (MoveStm (TempExp t) (CallExp f args)) (TempExp t):es)
  reorder (S t s1 s2) (e:es)
    | commute stm' e' = (SeqStm stm stm', e':es')
    | otherwise = t `seq`
      (SeqStm stm (SeqStm (MoveStm (TempExp t) e') stm'), TempExp t:es')
   where
    (stm, e') = doExp s1 e
    (stm', es') = reorder s2 es
  reorder _ [] = (ExpStm (ConstExp 0), [])

  reorderStm s l build = uncurry SeqStm $ build <$> reorder s l
  reorderExp s l build = build <$> reorder s l

  doStm :: Supply Temp -> Stm -> Stm
  doStm s (JumpStm e labs) = reorderStm s [e] (fn (`JumpStm` labs))
  doStm s (CJumpStm p a b t f) =
    reorderStm s [a, b] (fn (\a' b' -> CJumpStm p a' b' t f))
  doStm s (ExpStm (CallExp f args)) =
    reorderStm s (f:args) (fn ((ExpStm .) . CallExp))
  doStm s (ExpStm e) = reorderStm s [e] (fn ExpStm)
  doStm s (MoveStm (TempExp t) (CallExp f args)) =
    reorderStm s (f:args) (fn ((MoveStm (TempExp t) .) . CallExp))
  doStm s (MoveStm (TempExp t) b) = reorderStm s [b] (fn (MoveStm (TempExp t)))
  doStm s (MoveStm (MemExp e) b) = reorderStm s [e, b] (fn (MoveStm . MemExp))
  doStm s (MoveStm (ESeqExp stm e) b) = doStm s (SeqStm stm (MoveStm e b))
  doStm (S _ l r) (SeqStm a b) = SeqStm (doStm l a) (doStm r b)
  doStm _ stm = stm

  doExp :: Supply Temp -> Exp -> (Stm, Exp)
  doExp s (BinOpExp p a b) = reorderExp s [a, b] (fn (BinOpExp p))
  doExp s (MemExp a) = reorderExp s [a] (fn MemExp)
  doExp s (CallExp f args) = reorderExp s (f:args) (fn CallExp)
  doExp (S _ l r) (ESeqExp s e) = first (SeqStm (doStm l s)) (doExp r e)
  doExp s e = reorderExp s [] (const e)

  linear (SeqStm a b) l = linear a (linear b l)
  linear s l = s:l

basicBlocks :: Supply Label -> [Stm] -> ([[Stm]], Label)
basicBlocks (S !done _ s0) = (, done) . go s0
 where
  go :: Supply Label -> [Stm] -> [[Stm]]
  go _ [] = []
  go s (LabelStm l:rest) = (LabelStm l:block):go s rest'
   where (block, rest') = go' rest
  go (S l _ s) stms = go s (LabelStm l:stms)

  go' :: [Stm] -> ([Stm], [Stm])
  go' stms@(LabelStm l:_) = ([JumpStm (NameExp l) [l]], stms)
  go' (j@JumpStm{}:rest) = ([j], rest)
  go' (j@CJumpStm{}:rest) = ([j], rest)
  go' (stm:rest) = first (stm:) (go' rest)
  go' [] = ([JumpStm (NameExp done) [done]], [])

traceSchedule :: [[Stm]] -> Label -> [Stm]
traceSchedule = undefined

commute :: Stm -> Exp -> Bool
commute (ExpStm (ConstExp _)) _  = True
commute _ (NameExp _) = True
commute _ (ConstExp _) = True
commute _ _ = False