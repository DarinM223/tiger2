{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}
module Tiger.Liveness where

import Data.Foldable (foldl')
import Data.Maybe (fromMaybe, maybeToList)
import Tiger.Codegen (Instr (..))
import Tiger.Symbol (symbolId)
import Tiger.Temp (Temp (Temp, unTemp))
import qualified Data.Graph.Inductive as G
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS

data FlowGraph = FlowGraph
  { flowGraph  :: !(G.Gr String ())
  , flowDef    :: !(IM.IntMap IS.IntSet)
  , flowUse    :: !(IM.IntMap IS.IntSet)
  , flowIsMove :: !IS.IntSet
  } deriving (Show, Eq)

instr2graph :: [Instr] -> (FlowGraph, [G.Node])
instr2graph =
  go (0 :: Int) IM.empty [] (FlowGraph G.empty IM.empty IM.empty IS.empty) []
 where
  go !n !labelMap edges !g ns (LabelInstr _ lab:is) =
    go n (IM.insert (symbolId lab) n labelMap) edges g ns is
  go !n !labelMap edges !g ns (MoveInstr assem (Temp src) (Temp des):is)
    | src == des = go n labelMap edges g ns is
    | otherwise  = go (n + 1) labelMap edges g' (n:ns) is
   where
    c = ([((), n - 1) | n > 0], n, assem, [])
    g' = FlowGraph (c G.& flowGraph g)
      (IM.insert n (IS.fromList [des]) (flowDef g))
      (IM.insert n (IS.fromList [src]) (flowUse g))
      (IS.insert n (flowIsMove g))
  go !n !labelMap edges !g ns (OperInstr assem src des jmp:is) =
    go (n + 1) labelMap edges' g' (n:ns) is
   where
    edges' = fmap (n,) (fromMaybe [] jmp) ++ edges
    c = ([((), n - 1) | n > 0], n, assem, [])
    g' = g { flowGraph = c G.& flowGraph g
           , flowDef   = IM.insert n (IS.fromList $ fmap unTemp des) (flowDef g)
           , flowUse   = IM.insert n (IS.fromList $ fmap unTemp src) (flowUse g)
           }
  go _ !labelMap edges !g ns [] = (g', reverse ns)
   where
    !g' = g { flowGraph = G.insEdges edges' (flowGraph g) }
    edges' = edges >>= uncurry convertEdge
    convertEdge n =
      fmap (n,,()) . maybeToList . flip IM.lookup labelMap . symbolId

data IGraph = IGraph
  { iGraph :: G.Gr Temp ()
  , iMoves :: [(Int, Int)]
  }

interferenceGraph :: FlowGraph -> [G.Node] -> (IGraph, G.Node -> [Temp])
interferenceGraph g0 ns0 =
  (IGraph (foldl' buildGraph G.empty ns0) allMoves, lookupLiveOut)
 where
  FlowGraph{flowDef = def, flowUse = use, flowIsMove = isMove} = g0
  allMoves = (\n -> (headMap n def, headMap n use))
         <$> filter (`IS.member` isMove) ns0
  lookupLiveOut = fmap Temp . IS.toList . (liveMap IM.!)
  liveMap = buildLiveMap initMap initMap (flowGraph g0) (cycle ns0)

  initMap = IM.fromList $ fmap (,IS.empty) ns0
  buildLiveMap !inMap !outMap g (n:ns)
    | i == i' && o == o' = outMap'
    | otherwise          = buildLiveMap inMap' outMap' g ns
   where
    i = inMap IM.! n
    o = outMap IM.! n
    !i' = IS.union (use IM.! n) (IS.difference o (def IM.! n))
    !o' = IS.unions $ (inMap' IM.!) <$> G.suc g n
    inMap' = IM.insert n i' inMap
    outMap' = IM.insert n o' outMap
  buildLiveMap _ _ _ _ = error "Error: this shouldn't happen"

  headMap n = head . IS.toList . (IM.! n)
  buildGraph g n
    | IS.member n isMove = G.insEdges moveEdges g'
    | otherwise          = G.insEdges nonMoveEdges g'
   where
    g' = G.insNodes (toNode <$> defVars) g
    nonMoveEdges = [(a, b, ()) | a <- defVars, b <- liveOutVars]
    moveEdges = let c = headMap n use
                in [(a, b, ()) | a <- defVars, b <- liveOutVars, b /= c]
    defVars = IS.toList $ def IM.! n
    liveOutVars = IS.toList $ liveMap IM.! n
    toNode t = (t, Temp t)