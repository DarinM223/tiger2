{-# LANGUAGE BangPatterns #-}
module Tiger.RegAlloc where

import Data.Foldable (Foldable (foldl'))
import Tiger.Codegen (Codegen (codegen))
import Tiger.Color (Allocation, color)
import Tiger.Instr (Instr (..))
import Tiger.Liveness (instr2graph, interferenceGraph)
import Tiger.Temp (Supply (S), Temp (Temp), supplies)
import Tiger.Tree (Exp (TempExp), Stm (MoveStm))
import qualified Data.IntMap.Strict as IM
import qualified Tiger.Frame as F

rewrite
  :: (F.Frame frame, Codegen frame)
  => Supply (Temp, F.Access frame) -> [Instr] -> frame -> [Temp]
  -> [Instr]
rewrite s0 instrs0 frame = foldl' rewriteTemp instrs0 . zip (supplies s0)
 where
  rewriteTemp instrs (S (_, !access) _ s0', temp) = go (fmap fst s0') instrs
   where
    mem = F.exp access $ TempExp $ F.fp frame
    load s t = codegen s frame (MoveStm (TempExp t) mem)
    store s t = codegen s frame (MoveStm mem (TempExp t))

    go s (l@(LabelInstr _ _):rest) = l:go s rest
    go (S t1 (S t2 s1 s2) s3) (MoveInstr str src dest:rest) =
      before ++ [MoveInstr str src' dest'] ++ after ++ go s3 rest
     where
      before = if src == temp then load s1 t1 else []
      after = if dest == temp then store s2 t2 else []
      !src' = if src == temp then t1 else src
      !dest' = if dest == temp then t2 else dest
    go (S _ (S _ (S _ s1 s2) (S _ s3 s4)) s5) (OperInstr str srcs dests jmps:rest) =
      before ++ [OperInstr str srcs' dests' jmps] ++ after ++ go s5 rest
     where
      genTemps (S t' _ s) (t:ts)
        | t == temp = t' `seq` t':genTemps s ts
        | otherwise = t:genTemps s ts
      genTemps _ [] = []
      srcs' = genTemps s1 srcs
      dests' = genTemps s2 srcs
      before = concatMap (\(s, (_, t)) -> load s t) $
        zip (supplies s3) $ filter (uncurry (/=)) $ zip srcs srcs'
      after = concatMap (\(s, (_, t)) -> store s t) $
        zip (supplies s4) $ filter (uncurry (/=)) $ zip dests dests'
    go _ _ = []

alloc
  :: (F.Frame frame, Codegen frame)
  => Supply (Temp, F.Access frame) -> [Instr] -> frame
  -> ([Instr], Allocation)
alloc (S _ s1 s2) instrs frame
  | not (null spills) = alloc s1 (rewrite s2 instrs frame spills) frame
  | otherwise         = (filter (not . coalesced) instrs, alloc')
 where
  coalesced (MoveInstr _ (Temp src) (Temp dest)) =
    IM.lookup src alloc' == IM.lookup dest alloc'
  coalesced _ = False

  (g, ns) = instr2graph instrs
  (ig, _) = interferenceGraph g ns
  spillCost _ = 1
  (alloc', spills) = color ig (F.tempMap frame) spillCost (F.registers frame)