{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
module Tiger.Color where

import Prelude hiding (pred)
import Control.Monad.State.Strict
import Data.Foldable (foldl', for_, minimumBy, traverse_)
import Data.IntMap.Strict ((!))
import Data.Maybe (isJust)
import Data.Ord (comparing)
import GHC.Generics (Generic)
import Optics
import Optics.State.Operators
import Tiger.Frame (Register)
import Tiger.Liveness (IGraph (IGraph))
import Tiger.Temp (Temp (Temp))
import qualified Data.Graph.Inductive as G
import qualified Data.HashSet as HS
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM b t f = do b' <- b; if b' then t else f

whenM :: Monad m => m Bool -> m () -> m ()
whenM b t = ifM b t (pure ())

newtype F a = F a
instance Show (F a) where
  show _ = "F"
instance Eq (F a) where
  F _ == F _ = True

type Allocation = IM.IntMap Register

data ColorState = ColorState
  { adjSet           :: !(HS.HashSet (Int, Int))
  , adjList          :: !(IM.IntMap IS.IntSet)
  , degree           :: !(IM.IntMap Int)
  , colorAlloc       :: !Allocation
  , registers        :: !(HS.HashSet Register)
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
  , spillCost        :: F (Int -> Int)
  } deriving (Show, Eq, Generic)

mkColorState :: Int -> (Temp -> Int) -> ColorState
mkColorState k0 spillCost0 = ColorState
  HS.empty IM.empty IM.empty IM.empty HS.empty IM.empty IS.empty [] IS.empty
  IS.empty IS.empty IS.empty IS.empty IS.empty [] IM.empty HS.empty HS.empty
  HS.empty HS.empty HS.empty k0 (F (spillCost0 . Temp))

fromIGraph :: IGraph -> ColorState -> ColorState
fromIGraph (IGraph gr moves) = addMoves . addEdges gr . execState (addNodes gr)
 where
  addMoves s0 = foldl' (\s m -> execState (addMove m) s) s0 moves

  addMove :: (Int, Int) -> State ColorState ()
  addMove m@(a, b) = do
    precolored' <- use #precolored
    unless (IS.member a precolored') $
      #moveList % at' a % _Just %= HS.insert m
    unless (IS.member b precolored') $
      #moveList % at' b % _Just %= HS.insert m
    #worklistMoves %= HS.insert m

  addEdges g s | G.isEmpty g = s
  addEdges (G.matchAny -> ((pred, n, _, suc), g)) s0 = addEdges g $
    foldl' (\s (u, v) -> addEdge u v s) s0 (fmap ((n ,) . snd) (pred ++ suc))

  addNodes :: G.Gr Temp () -> State ColorState ()
  addNodes g | G.isEmpty g = pure ()
  addNodes (G.matchAny -> ((_, n, _, _), g)) = do
    use #colorAlloc >>= \c -> case IM.lookup n c of
      Just _  -> #precolored %= IS.insert n
      Nothing -> #initial %= (n :)
    #adjList %= IM.insert n IS.empty
    #degree %= IM.insert n 0
    #moveList %= IM.insert n HS.empty
    addNodes g

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
  #alias % at' v ?= u
  use (#moveList % at' v) >>=
    traverse_ (\vMoves -> #moveList % at' u % _Just %= HS.union vMoves)
  adj <- gets $ adjacent v
  for_ (IS.elems adj) $ \t -> do
    modify' $ addEdge t u
    decrementDegree t
  whenM (gets $ \s -> degree s ! u >= k s && IS.member u (freezeWorklist s)) $ do
    #freezeWorklist %= IS.delete u
    #spillWorklist %= IS.insert u

freeze :: State ColorState ()
freeze = do
  u <- #freezeWorklist %%= IS.deleteFindMin
  #simplifyWorklist %= IS.insert u
  freezeMoves u

freezeMoves :: Int -> State ColorState ()
freezeMoves u = do
  moves <- gets $ nodeMoves u
  for_ (HS.toList moves) $ \m@(x, y) -> do
    v <- gets $ \s -> if getAlias y s == getAlias u s
      then getAlias x s
      else getAlias y s
    #activeMoves %= HS.delete m
    #frozenMoves %= HS.insert m
    whenM (gets $ \s -> HS.null (nodeMoves v s) && degree s ! v < k s) $ do
      #freezeWorklist %= IS.delete v
      #simplifyWorklist %= IS.insert v

selectSpill :: State ColorState ()
selectSpill = do
  F spillCost' <- use #spillCost
  m <- minimumBy (comparing spillCost') . IS.elems <$> use #spillWorklist
  #spillWorklist %= IS.delete m
  #simplifyWorklist %= IS.insert m
  freezeMoves m

assignColors :: ColorState -> ColorState
assignColors s0 =
  colorCoalesced $ foldl' go s0 { selectStack = [] } $ selectStack s0
 where
  go s n = case HS.toList okColors of
    (c:_) -> s & #coloredNodes %~ IS.insert n
               & #colorAlloc % at' n ?~ c
    [] -> s & #spilledNodes %~ IS.insert n
   where
    okColors = foldl'
      (\colors w ->
        if IS.member (getAlias w s) (IS.union (coloredNodes s) (precolored s))
          then HS.delete (colorAlloc s ! getAlias w s) colors
          else colors)
      (registers s) (IS.elems (adjList s ! n))
  colorCoalesced s = foldl'
    (\s' n -> s' & #colorAlloc % at' n ?~ colorAlloc s' ! getAlias n s')
    s (IS.elems (coalescedNodes s))

loop :: ColorState -> ColorState
loop s
  | not (IS.null (simplifyWorklist s)) = loop $ execState simplify s
  | not (HS.null (worklistMoves s))    = loop $ execState coalesce s
  | not (IS.null (freezeWorklist s))   = loop $ execState freeze s
  | not (IS.null (spillWorklist s))    = loop $ execState selectSpill s
  | otherwise                          = s

color :: IGraph -> Allocation -> (Temp -> Int) -> [Register] -> (Allocation, [Temp])
color interference initAlloc spillCost' regs =
  (colorAlloc s', Temp <$> IS.elems (spilledNodes s'))
 where
  k' = length regs
  s = (mkColorState k' spillCost')
    { colorAlloc = initAlloc, registers = HS.fromList regs }
  s' = assignColors . loop . makeWorklist $ fromIGraph interference s