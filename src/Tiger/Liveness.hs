{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}
module Tiger.Liveness where

import Data.Foldable (foldl')
import Data.IntMap.Strict ((!))
import Data.Maybe (fromMaybe, maybeToList)
import Tiger.Assem (Instr (..))
import Tiger.Symbol (symbolId)
import Tiger.Temp (Temp (Temp, unTemp))
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS

type Node = Int
type Graph = IM.IntMap IS.IntSet

insEdge :: Int -> Int -> Graph -> Graph
insEdge u v = IM.alter (Just . maybe (IS.singleton v) (IS.insert v)) u

data FlowGraph = FlowGraph
  { flowGraph  :: !Graph
  , flowDef    :: !(IM.IntMap IS.IntSet)
  , flowUse    :: !(IM.IntMap IS.IntSet)
  , flowIsMove :: !IS.IntSet
  } deriving (Show, Eq)

instr2graph :: [Instr] -> (FlowGraph, [Node])
instr2graph =
  go (0 :: Int) IM.empty [] (FlowGraph IM.empty IM.empty IM.empty IS.empty) []
 where
  -- Only add edge from the node to the next node if
  -- the node has no jumps and the node isn't the last instruction.
  addNode n Nothing (_:_) = IM.insert n (IS.singleton (n + 1))
  addNode n _ _           = IM.insert n IS.empty

  go !n !labelMap edges !g ns is@[LabelInstr _ _] =
    go n labelMap edges g ns (is ++ [OperInstr "" [] [] Nothing])
  go !n !labelMap edges !g ns (LabelInstr _ lab:is) =
    go n (IM.insert (symbolId lab) n labelMap) edges g ns is
  go !n !labelMap edges !g ns (MoveInstr _ (Temp src) (Temp des):is) =
    go (n + 1) labelMap edges g' (n:ns) is
   where
    g' = FlowGraph (addNode n Nothing is (flowGraph g))
      (IM.insert n (IS.fromList [des]) (flowDef g))
      (IM.insert n (IS.fromList [src]) (flowUse g))
      (IS.insert n (flowIsMove g))
  go !n !labelMap edges !g ns (OperInstr _ src des jmp:is) =
    go (n + 1) labelMap edges' g' (n:ns) is
   where
    edges' = fmap (n,) (fromMaybe [] jmp) ++ edges
    g' = g { flowGraph = addNode n jmp is (flowGraph g)
           , flowDef   = IM.insert n (IS.fromList $ fmap unTemp des) (flowDef g)
           , flowUse   = IM.insert n (IS.fromList $ fmap unTemp src) (flowUse g)
           }
  go _ !labelMap edges !g ns [] = (g', ns)
   where
    !g' = g { flowGraph = foldl' (flip (uncurry insEdge)) (flowGraph g) edges' }
    edges' = edges >>= uncurry convertEdge
    convertEdge n = fmap (n,) . maybeToList . flip IM.lookup labelMap . symbolId

data IGraph = IGraph
  { iGraph :: !Graph
  , iMoves :: [(Int, Int)]
  } deriving (Show, Eq)

calcLive :: FlowGraph -> [Node] -> (IM.IntMap IS.IntSet, IM.IntMap IS.IntSet)
calcLive (FlowGraph g def use _) ns0 = buildLiveMap initMap initMap ns0
 where
  initMap = IM.fromList $ fmap (,IS.empty) ns0
  buildLiveMap !inMap0 !outMap0 ns
    | inMap0 == inMap0' && outMap0 == outMap0' = (inMap0', outMap0')
    | otherwise = buildLiveMap inMap0' outMap0' ns
   where
    (inMap0', outMap0') = foldl' go (inMap0, outMap0) ns
    go (!inMap, !outMap) n = (inMap', outMap')
     where
      !i' = IS.union (use ! n) (IS.difference (outMap ! n) (def ! n))
      !o' = IS.unions $ (inMap !) <$> IS.elems (g ! n)
      inMap' = IM.insert n i' inMap
      outMap' = IM.insert n o' outMap

interferenceGraph :: FlowGraph -> [Node] -> (IGraph, Node -> [Temp])
interferenceGraph g0 ns0 =
  (IGraph (foldl' buildGraph IM.empty (reverse ns0)) allMoves, lookupLiveOut)
 where
  FlowGraph{flowDef = def, flowUse = use, flowIsMove = isMove} = g0
  allMoves = (\n -> (head (IS.elems (def ! n)), head (IS.elems (use ! n))))
         <$> filter (`IS.member` isMove) ns0
  lookupLiveOut = fmap Temp . IS.toList . (liveMap !)
  (_, liveMap) = calcLive g0 ns0

  insNodes = flip $ foldl' (flip (IM.alter (Just . fromMaybe IS.empty)))
  insEdges = flip $ foldl' (flip (\(u, v) -> insEdge v u . insEdge u v))
  buildGraph g n
    | IS.member n isMove = insEdges moveEdges g'
    | otherwise          = insEdges nonMoveEdges g'
   where
    g' = insNodes (defVars ++ useVars) g
    nonMoveEdges = [(a, b) | a <- defVars, b <- liveOutVars, a /= b]
    moveEdges = let c = head useVars
                in filter (\(_, b) -> b /= c) nonMoveEdges
    defVars = IS.toList $ def ! n
    useVars = IS.toList $ use ! n
    liveOutVars = IS.toList $ liveMap ! n