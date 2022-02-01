{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
module Tiger.Color where

import Prelude hiding (pred)
import Control.Monad.Extra
import Control.Monad.State.Strict
import Data.Foldable (foldl', for_, traverse_)
import Data.IntMap.Strict ((!))
import Data.Maybe (isJust)
import GHC.Generics (Generic)
import Optics
import Optics.State.Operators
import Tiger.Liveness (IGraph (IGraph))
import qualified Data.Graph.Inductive as G
import qualified Data.HashSet as HS
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS

data ColorState = ColorState
  { adjSet           :: !(HS.HashSet (Int, Int))
  , adjList          :: !(IM.IntMap IS.IntSet)
  , degree           :: !(IM.IntMap Int)
  , color            :: !(IM.IntMap Int)
  , alias            :: !(IM.IntMap Int)
  , precolored       :: !IS.IntSet
  , initial          :: [Int]
  , simplifyWorklist :: !IS.IntSet
  , freezeWorklist   :: !IS.IntSet
  , spillWorklist    :: !IS.IntSet
  , spilledNodes     :: !IS.IntSet
  , coalescedNodes   :: !IS.IntSet
  , coloredNodes     :: !IS.IntSet
  , selectStack      :: [Int]
  , moveList         :: !(IM.IntMap (HS.HashSet (Int, Int)))
  , coalescedMoves   :: !(HS.HashSet (Int, Int))
  , constrainedMoves :: !(HS.HashSet (Int, Int))
  , frozenMoves      :: !(HS.HashSet (Int, Int))
  , worklistMoves    :: !(HS.HashSet (Int, Int))
  , activeMoves      :: !(HS.HashSet (Int, Int))
  , k                :: !Int
  } deriving (Show, Eq, Generic)

mkColorState :: Int -> ColorState
mkColorState = ColorState
  HS.empty IM.empty IM.empty IM.empty IM.empty IS.empty [] IS.empty IS.empty
  IS.empty IS.empty IS.empty IS.empty [] IM.empty HS.empty HS.empty HS.empty
  HS.empty HS.empty

fromIGraph :: IGraph -> ColorState -> ColorState
fromIGraph (IGraph gr moves) = addMoves . buildGraph gr
 where
  addMoves s0 = foldl' addMove s0 moves
  addMove s m@(a, b) = s { moveList = moveList' }
   where
    moveList' = IM.adjust (HS.insert m) a
              . IM.adjust (HS.insert m) b
              $ moveList s
  buildGraph g s | G.isEmpty g = s
  buildGraph (G.matchAny -> ((pred, n, _, suc), g)) s0 = buildGraph g $
    foldl' (\s (u, v) -> addEdge u v s) s' (fmap ((n ,) . snd) (pred ++ suc))
   where s' = addNode n s0

addNode :: Int -> ColorState -> ColorState
addNode n s = s { adjList  = IM.insert n IS.empty (adjList s)
                , degree   = IM.insert n 0 (degree s)
                , moveList = IM.insert n HS.empty (moveList s)
                }

addEdge :: Int -> Int -> ColorState -> ColorState
addEdge u v s
  | HS.member (u, v) (adjSet s) && u /= v =
    updateNode u v $ updateNode v u s { adjSet = adjSet' }
  | otherwise = s
 where
  adjSet' = HS.insert (u, v) $ HS.insert (v, u) $ adjSet s
  updateNode a b s'
    | not (IS.member a (precolored s')) = s'
      { adjList = IM.adjust (IS.insert b) a (adjList s')
      , degree  = IM.adjust (+ 1) a (degree s')
      }
    | otherwise = s'

makeWorklist :: ColorState -> ColorState
makeWorklist s0 = foldl' build s0 { initial = [] } (initial s0)
 where
  build s n
    | degree s ! n >= k s = s & #spillWorklist %~ IS.insert n
    | moveRelated n s = s & #freezeWorklist %~ IS.insert n
    | otherwise = s & #simplifyWorklist %~ IS.insert n

adjacent :: Int -> ColorState -> IS.IntSet
adjacent n s = IS.difference
  (adjList s ! n)
  (IS.union (IS.fromList (selectStack s)) (coalescedNodes s))

nodeMoves :: Int -> ColorState -> HS.HashSet (Int, Int)
nodeMoves n s = HS.intersection
  (moveList s ! n)
  (HS.union (activeMoves s) (worklistMoves s))

moveRelated :: Int -> ColorState -> Bool
moveRelated n s = not $ HS.null $ nodeMoves n s

simplify :: State ColorState ()
simplify = do
  n <- #simplifyWorklist %%= IS.deleteFindMin
  #selectStack %= (n :)
  adj <- gets $ adjacent n
  traverse_ decrementDegree $ IS.elems adj

decrementDegree :: Int -> State ColorState ()
decrementDegree m = do
  k' <- use #k
  d <- use (#degree % at' m)
  #degree % at' m % _Just %= subtract 1
  when (d == Just k') $ do
    adj <- gets $ adjacent m
    enableMoves $ IS.insert m adj
    #spillWorklist %= IS.delete m
    ifM (gets $ moveRelated m)
      (#freezeWorklist %= IS.insert m)
      (#simplifyWorklist %= IS.insert m)

enableMoves :: IS.IntSet -> State ColorState ()
enableMoves nodes =
  for_ (IS.elems nodes) $ \n -> do
    moves <- gets $ nodeMoves n
    for_ (HS.toList moves) $ \m ->
      ifM (HS.member m <$> use #activeMoves)
        (#activeMoves %= HS.delete m)
        (#worklistMoves %= HS.insert m)

coalesce :: State ColorState ()
coalesce = do
  precolored' <- use #precolored
  m@(x, y) <- head . HS.toList <$> use #worklistMoves
  x' <- gets $ getAlias x
  y' <- gets $ getAlias y
  let (u, v) = if IS.member y precolored' then (y', x') else (x', y')
  #worklistMoves %= HS.delete m

  adjSet' <- use #adjSet
  case () of
    _ | u == v -> do
      #coalescedMoves %= HS.insert m
      modify' $ addWorklist u
    _ | IS.member v precolored' || HS.member (u, v) adjSet' -> do
      #constrainedMoves %= HS.insert m
      modify' $ addWorklist u
      modify' $ addWorklist v
    _ ->
      let
        cond s = (IS.member u precolored' &&
                  all (\t -> ok t u s) (IS.elems (adjacent v s)))
              || (not (IS.member u precolored') &&
                  conservative (IS.union (adjacent u s) (adjacent v s)) s)
      in
      ifM (gets cond)
        (do
          #coalescedMoves %= HS.insert m
          combine u v
          modify' $ addWorklist u)
        (#activeMoves %= HS.insert m)

addWorklist :: Int -> ColorState -> ColorState
addWorklist u s
  | not (IS.member u (precolored s)) &&
      not (moveRelated u s) && degree s ! u < k s =
    s & #freezeWorklist %~ IS.delete u
      & #simplifyWorklist %~ IS.insert u
  | otherwise = s

ok :: Int -> Int -> ColorState -> Bool
ok t r s =
  degree s ! t < k s || IS.member t (precolored s) || HS.member (t, r) (adjSet s)

conservative :: IS.IntSet -> ColorState -> Bool
conservative nodes s =
  length (filter (\n -> degree s ! n >= k s) (IS.elems nodes)) < k s

getAlias :: Int -> ColorState -> Int
getAlias n s | IS.member n (coalescedNodes s) = getAlias (alias s IM.! n) s
             | otherwise                      = n

combine :: Int -> Int -> State ColorState ()
combine u v = do
  r <- use (#freezeWorklist % at' v)
  if isJust r
    then #freezeWorklist %= IS.delete v
    else #spillWorklist %= IS.delete v
  #coalescedNodes %= IS.insert v
  #alias % at' v % _Just .= u
  use (#moveList % at' v) >>=
    traverse_ (\vMoves -> #moveList % at' u % _Just %= HS.union vMoves)
  adj <- gets $ adjacent v
  for_ (IS.elems adj) $ \t -> do
    modify' $ addEdge t u
    decrementDegree t
  whenM (gets $ \s -> degree s ! u >= k s && IS.member u (freezeWorklist s)) $ do
    #freezeWorklist %= IS.delete u
    #spillWorklist %= IS.insert u