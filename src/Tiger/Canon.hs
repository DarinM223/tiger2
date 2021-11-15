{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
module Tiger.Canon where

import Data.Bifunctor (first)
import Tiger.Temp (Label, Supply (S), Temp)
import Tiger.Tree
import Tiger.Types (insertEnv, lookupEnv)
import qualified Data.IntMap as IM

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
  go (S !l _ s) stms = go s (LabelStm l:stms)

  go' :: [Stm] -> ([Stm], [Stm])
  go' stms@(LabelStm l:_) = ([JumpStm (NameExp l) [l]], stms)
  go' (j@JumpStm{}:rest) = ([j], rest)
  go' (j@CJumpStm{}:rest) = ([j], rest)
  go' (stm:rest) = first (stm:) (go' rest)
  go' [] = ([JumpStm (NameExp done) [done]], [])

traceSchedule :: Supply Label -> [[Stm]] -> Label -> [Stm]
traceSchedule s0 blocks done =
  getNext s0 (foldr enterBlock IM.empty blocks) blocks ++ [LabelStm done]
 where
  getNext s table (b@(LabelStm lab:_):rest)
    | Just (_:_) <- lookupEnv lab table = trace s table b rest
    | otherwise = getNext s table rest
  getNext _ _ _ = []

  trace s table0 b@(LabelStm lab0:_) rest = case splitLast b of
    (most, JumpStm (NameExp lab) _) -> case lookupEnv lab table of
      Just b'@(_:_) -> most ++ trace s table b' rest
      _             -> b ++ getNext s table rest
    (most, CJumpStm opr x y t f) ->
      case (lookupEnv t table, lookupEnv f table) of
        (_, Just b'@(_:_)) -> b ++ trace s table b' rest
        (Just b'@(_:_), _) ->
          most ++ [CJumpStm (notRel opr) x y f t] ++ trace s table b' rest
        _ -> let S !f' _ s' = s in most
          ++ [CJumpStm opr x y t f', LabelStm f', JumpStm (NameExp f) [f]]
          ++ getNext s' table rest
    (_, JumpStm _ _) -> b ++ getNext s table rest
    _ -> error "Invalid last statement in trace"
   where table = insertEnv lab0 [] table0
  trace _ _ _ _ = error "Invalid statement for trace"

  enterBlock b@(LabelStm s:_) = insertEnv s b
  enterBlock _                = id

  splitLast [x]   = ([], x)
  splitLast (h:t) = first (h:) (splitLast t)
  splitLast _     = error "splitLast list must be non-empty"

commute :: Stm -> Exp -> Bool
commute (ExpStm (ConstExp _)) _  = True
commute _ (NameExp _) = True
commute _ (ConstExp _) = True
commute _ _ = False