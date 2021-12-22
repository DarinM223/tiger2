{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}
module Tiger.Liveness where

import Data.Maybe (fromMaybe, maybeToList)
import Tiger.Codegen (Instr (..))
import Tiger.Symbol (symbolId)
import Tiger.Temp (Temp)
import qualified Data.Graph.Inductive as G
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS

data FlowGraph = FlowGraph
  { flowGraph  :: !(G.Gr String ())
  , flowDef    :: !(IM.IntMap [Temp])
  , flowUse    :: !(IM.IntMap [Temp])
  , flowIsMove :: !IS.IntSet
  } deriving (Show, Eq)

instr2graph :: [Instr] -> (FlowGraph, [G.Node])
instr2graph =
  go (0 :: Int) IM.empty [] (FlowGraph G.empty IM.empty IM.empty IS.empty) []
 where
  go !n !labelMap edges !g ns (LabelInstr _ lab:is) =
    go n (IM.insert (symbolId lab) n labelMap) edges g ns is
  go !n !labelMap edges !g ns (MoveInstr assem src des:is)
    | src == des = go n labelMap edges g ns is
    | otherwise  = go (n + 1) labelMap edges g' (n:ns) is
   where
    c = ([((), n - 1) | n > 0], n, assem, [])
    g' = FlowGraph (c G.& flowGraph g)
      (IM.insert n [des] (flowDef g))
      (IM.insert n [src] (flowUse g))
      (IS.insert n (flowIsMove g))
  go !n !labelMap edges !g ns (OperInstr assem src des jmp:is) =
    go (n + 1) labelMap edges' g' (n:ns) is
   where
    edges' = fmap (n,) (fromMaybe [] jmp) ++ edges
    c = ([((), n - 1) | n > 0], n, assem, [])
    g' = g { flowGraph = c G.& flowGraph g
           , flowDef   = IM.insert n des (flowDef g)
           , flowUse   = IM.insert n src (flowUse g)
           }
  go _ !labelMap edges !g ns [] = (g', reverse ns)
   where
    !g' = g { flowGraph = G.insEdges edges' (flowGraph g) }
    edges' = edges >>= uncurry convertEdge
    convertEdge n =
      fmap (n,,()) . maybeToList . flip IM.lookup labelMap . symbolId