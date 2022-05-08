module Tiger.RegAlloc where

import Data.Foldable (Foldable (foldl'), foldlM)
import Tiger.Assem (Instr (..))
import Tiger.Codegen (Codegen (codegen))
import Tiger.Color (Allocation, color)
import Tiger.Liveness
  (FlowGraph (FlowGraph), IGraph (IGraph), instr2graph, interferenceGraph)
import Tiger.Temp (Supply (S), Temp (Temp), supplies)
import Tiger.Tree (Exp (TempExp), Stm (MoveStm))
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import qualified Tiger.Frame as F

rewrite
  :: (Monad m, F.Frame frame, Codegen frame)
  => Supply Temp -> m (F.Access frame) -> [Instr] -> frame -> [Temp]
  -> m [Instr]
rewrite s0 mkLocal instrs0 frame = foldlM rewriteTemp instrs0 . zip (supplies s0)
 where
  rewriteTemp instrs (s0', temp) = do
    access <- mkLocal
    let
      mem = F.exp access $ TempExp $ F.fp frame
      load s t = codegen s frame (MoveStm (TempExp t) mem)
      store s t = codegen s frame (MoveStm mem (TempExp t))

      go s (l@(LabelInstr _ _):rest) = l:go s rest
      go (S t1 (S t2 s1 s2) s3) (MoveInstr str src dest:rest) =
        before ++ [MoveInstr str src' dest'] ++ after ++ go s3 rest
       where
        before = if src == temp then load s1 t1 else []
        after = if dest == temp then store s2 t2 else []
        src' = if src == temp then t1 else src
        dest' = if dest == temp then t2 else dest
      go s0'' (OperInstr str srcs dests jmps:rest) =
        before ++ [OperInstr str srcs' dests' jmps] ++ after ++ go s5 rest
       where
        (S _ (S _ (S _ s1 s2) (S _ s3 s4)) s5) = s0''
        genTemps s (t:ts)
          | t == temp = let S !t' _ s' = s in t':genTemps s' ts
          | otherwise = t:genTemps s ts
        genTemps _ [] = []
        srcs' = genTemps s1 srcs
        dests' = genTemps s2 dests
        before = concatMap (\(s, (_, t)) -> load s t) $
          zip (supplies s3) $ filter (uncurry (/=)) $ zip srcs srcs'
        after = concatMap (\(s, (_, t)) -> store s t) $
          zip (supplies s4) $ filter (uncurry (/=)) $ zip dests dests'
      go _ _ = []
    return $ go s0' instrs

spillCost :: FlowGraph -> IGraph -> Temp -> Double
spillCost (FlowGraph g def use _) (IGraph ig _) = cost
 where
  cost (Temp temp) =
    fromIntegral (outsideLoop + 10 * insideLoop) / fromIntegral degree
   where
    degree = IS.size $ ig IM.! temp
    (outsideLoop, insideLoop) = foldl' build (0, 0) $ IM.keys g
    build (!outside, !inside) n
      | IS.member n inLoop = (outside, inside + count def + count use)
      | otherwise          = (outside + count def + count use, inside)
     where count m = length $ filter (== temp) $ IS.elems (m IM.! n)

  inLoop = dfs IS.empty [0]
  dfs visited (n:path)
    | IS.member n visited =
      foldl' (flip IS.insert) (IS.singleton n) $ takeWhile (/= n) path
    | otherwise =
      IS.unions $ (\n' -> dfs visited' (n':n:path)) <$> IS.elems (g IM.! n)
   where visited' = IS.insert n visited
  dfs _ [] = IS.empty

alloc
  :: (Monad m, F.Frame frame, Codegen frame)
  => Supply Temp -> m (F.Access frame) -> [Instr] -> frame
  -> m ([Instr], Allocation)
alloc (S _ s1 s2) mkLocal instrs frame
  | not (null spills) = do
    instrs' <- rewrite s1 mkLocal instrs frame spills
    alloc s2 mkLocal instrs' frame
  | otherwise = pure (filter (not . coalesced) instrs, alloc')
 where
  coalesced (MoveInstr _ (Temp src) (Temp dest)) =
    IM.lookup src alloc' == IM.lookup dest alloc'
  coalesced _ = False

  (g, ns) = instr2graph instrs
  (ig, _) = interferenceGraph g ns
  (alloc', spills) =
    color ig (F.tempMap frame) (spillCost g ig) (IM.elems (F.tempMap frame))