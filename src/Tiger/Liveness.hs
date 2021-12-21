{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}
module Tiger.Liveness where

import Data.Maybe (fromMaybe, maybeToList)
import Tiger.Codegen (Instr (..))
import Tiger.Symbol (symbolId)
import Tiger.Temp (Temp)
import qualified Data.Graph.Inductive as G
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS

data FlowGraph = FlowGraph
  { flowGraph  :: G.Gr String ()
  , flowDef    :: IM.IntMap [Temp]
  , flowUse    :: IM.IntMap [Temp]
  , flowIsMove :: IS.IntSet
  } deriving (Show, Eq)

instr2graph :: [Instr] -> (FlowGraph, [G.Node])
instr2graph = go 0 IM.empty
 where
  go :: Int -> IM.IntMap G.Node -> [Instr] -> (FlowGraph, [G.Node])
  go n labelMap (LabelInstr _ lab:is) = (g, ns)
   where (g, ns) = go n (IM.insert (symbolId lab) (head ns) labelMap) is
  go !n labelMap (MoveInstr assem src des:is)
    | src == des = (g, ns)
    | otherwise  = (g', n:ns)
   where
    (g, ns) = go (n + 1) labelMap is
    c = ([((), n - 1) | n > 0], n, assem, [])
    g' = FlowGraph (c G.& flowGraph g)
      (IM.insert n [des] (flowDef g))
      (IM.insert n [src] (flowUse g))
      (IS.insert n (flowIsMove g))
  go !n labelMap (OperInstr assem src des jmp:is) = (g', n:ns)
   where
    (g, ns) = go (n + 1) labelMap is
    suc = fromMaybe [] jmp >>= maybeToList . flip IM.lookup labelMap . symbolId
    c = ([((), n - 1) | n > 0], n, assem, fmap ((),) suc)
    g' = g { flowGraph = c G.& flowGraph g
           , flowDef   = IM.insert n des (flowDef g)
           , flowUse   = IM.insert n src (flowUse g)
           }
  go _ _ [] = (FlowGraph G.empty IM.empty IM.empty IS.empty, [])