{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
module Tiger.Translate where

import Prelude hiding (exp)
import Control.Applicative (Applicative (liftA2))
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Tiger.Temp (Label, Temp_ (..), Unique)
import Tiger.Tree hiding (Exp)
import qualified Tiger.AST as AST
import qualified Tiger.Frame as F
import qualified Tiger.Tree as Tree

data Exp = Ex Tree.Exp
         | Nx Tree.Stm
         | Cx (Label -> Label -> Tree.Stm)

instance Show Exp where
  show (Ex e) = show e
  show (Nx e) = show e
  show (Cx _) = "Cx"

unit :: Exp
unit = Ex $ ConstExp 0

type Access level = (level, F.Access (Frame level))

class Translate level where
  type Frame level
  outermost :: level
  formals   :: level -> [Access level]
  simpleVar :: Access level -> level -> Exp

data Translate_ level m = Translate_
  { newLevel     :: level -> Label -> [Bool] -> m level
  , allocLocal   :: level -> Bool -> m (Access level)

  , subscriptVar :: Exp -> Exp -> m Exp
  , fieldVar     :: Exp -> Label -> [Label] -> m Exp

  , intExp       :: Int -> m Exp
  , stringExp    :: String -> m Exp
  , recordExp    :: [Exp] -> m Exp
  , arrayExp     :: Exp -> Exp -> m Exp
  , binOpExp     :: AST.Op -> Exp -> Exp -> m Exp
  , iRelOpExp    :: AST.Op -> Exp -> Exp -> m Exp
  , sRelOpExp    :: AST.Op -> Exp -> Exp -> m Exp
  , ifElseExp    :: Exp -> Exp -> Maybe Exp -> m Exp
  , whileExp     :: Exp -> Exp -> Label -> m Exp
  , breakExp     :: Label -> m Exp
  , funCallExp   :: level -> level -> Label -> [Exp] -> m Exp
  , letExp       :: [Exp] -> Exp -> m Exp
  , seqExp       :: [Exp] -> m Exp
  , assignExp    :: Exp -> Exp -> m Exp

  , functionDec  :: level -> Exp -> m ()
  }

data Frag frame = ProcFrag Stm frame
                | StringFrag Label String
                deriving Show

data Level frame
  = Outermost
  | Level
  { levelParent  :: Level frame
  , levelFrame   :: frame
  , levelFormals :: [Access (Level frame)]
  , levelUnique  :: Unique
  }

instance Eq (Level frame) where
  Outermost == Outermost = True
  Outermost == _ = False
  _ == Outermost = False
  l1 == l2 = levelUnique l1 == levelUnique l2

instance F.Frame frame => Translate (Level frame) where
  type Frame (Level frame) = frame
  outermost = Outermost
  formals Outermost = []
  formals l = tail (levelFormals l)
  simpleVar (lg, access) lf = Ex $ F.exp access $ staticLinks lf lg

staticLinks :: F.Frame frame => Level frame -> Level frame -> Tree.Exp
staticLinks lf lg = go (TempExp (F.fp (levelFrame lf))) lf
 where
  go build lf' | lg == lf' = build
  go build lf' = go (F.exp staticLink build) (levelParent lf')
   where (_, staticLink) = head $ levelFormals lf'

translate_
  :: forall frame m. (Monad m, F.Frame frame)
  => Temp_ m -> m Unique -> (Frag frame -> m ()) -> F.Frame_ frame m
  -> Translate_ (Level frame) m
translate_ Temp_{..} unique put f_ =
  let
    -- A way of thinking of the implementation of unEx, unNx, and unCx
    -- is that for any A and B, `unA (B _)` translates to "convert B into A".
    --
    -- Another thing to think about is that conditionals always return some form
    -- of jump statement or collection of jump statements.
    --
    -- So the case of `unNx (Cx _)` would be converting a conditional
    -- into a statement which swallows the jump.
    --
    -- The case of `unCx (Ex _)` would be converting an expression
    -- into a conditional that returns a jump statement that
    -- compares the expression to 0 (false).
    --
    -- With unNx you want to swallow any expressions or jumps.
    -- With unCx you want to turn everything into a function that returns a jump.

    unEx :: Exp -> m Tree.Exp
    unEx (Ex e) = pure e
    unEx (Nx s) = pure $ ESeqExp s (ConstExp 0)
    unEx (Cx genStm) = do
      r <- newTemp
      t <- newLabel
      f <- newLabel
      let branchTemp = stmSeq
            [ MoveStm (TempExp r) (ConstExp 1)
            , genStm t f
            , LabelStm f
            , MoveStm (TempExp r) (ConstExp 0)
            , LabelStm t
            ]
      pure $ ESeqExp branchTemp (TempExp r)

    unNx :: Exp -> m Tree.Stm
    unNx (Ex e) = pure $ ExpStm e
    unNx (Nx s) = pure s
    unNx (Cx genStm) = do
      t <- newLabel
      pure $ SeqStm (genStm t t) (LabelStm t)

    unCx :: Exp -> Label -> Label -> Tree.Stm
    unCx (Ex (ConstExp 0)) = \_ f -> JumpStm (NameExp f) [f]
    unCx (Ex (ConstExp 1)) = \t _ -> JumpStm (NameExp t) [t]
    unCx (Ex e) = flip (CJumpStm Eq e (ConstExp 0))
    unCx (Cx genStm) = genStm
    unCx (Nx _) = error "Calling unCx on an Nx constructor"

    newLevel parent name escapes = do
      frame <- F.newFrame f_ name (True:escapes)
      u <- unique
      let level = Level parent frame ((level ,) <$> F.formals frame) u
      pure level
    allocLocal Outermost _ = error "Calling allocLocal on an Outermost level"
    allocLocal level escapes =
      (level ,) <$> F.allocLocal f_ (levelFrame level) escapes

    subscriptVar e i = do
      e' <- unEx e
      i' <- unEx i
      pure $ Ex $ MemExp $ BinOpExp Plus e' $
        BinOpExp Mul i' $ ConstExp (F.wordSize @frame)
    fieldVar e f fs = do
      e' <- unEx e
      pure $ Ex $ MemExp $ BinOpExp Plus e' $
        BinOpExp Mul i $ ConstExp (F.wordSize @frame)
     where i = ConstExp $ fromJust $ elemIndex f fs

    intExp = pure . Ex . ConstExp
    stringExp lit = do
      label <- newLabel
      put $ StringFrag label lit
      pure $ Ex $ NameExp label
    recordExp exps = do
      callExp <- F.externalCall f_ "malloc" [ConstExp size]
      exps' <- traverse unEx exps
      temp <- newTemp
      let body = stmSeq
            [ MoveStm (TempExp temp) callExp
            , stmSeq $ move (TempExp temp) <$> zip exps' ([0..] :: [Int])
            ]
      pure $ Ex $ ESeqExp body (TempExp temp)
     where
      size = length exps * F.wordSize @frame
      move temp (e, i) = MoveStm
        (MemExp (BinOpExp Plus temp (ConstExp (i * F.wordSize @frame)))) e
    arrayExp len initExp = do
      params <- (\l e -> [l, e]) <$> unEx len <*> unEx initExp
      Ex <$> F.externalCall f_ "initArray" params
    binOpExp op e1 e2 = Ex <$> liftA2 (BinOpExp op') (unEx e1) (unEx e2)
     where
      op' = case op of
        AST.AddOp -> Tree.Plus
        AST.SubOp -> Tree.Minus
        AST.MulOp -> Tree.Mul
        AST.DivOp -> Tree.Div
        AST.AndOp -> Tree.And
        AST.OrOp  -> Tree.Or
        _         -> error "Invalid operator for binOp"
    iRelOpExp op e1 e2 = Cx <$> liftA2 (CJumpStm op') (unEx e1) (unEx e2)
     where
      op' = case op of
        AST.EqOp  -> Tree.Eq
        AST.NeqOp -> Tree.Ne
        AST.LtOp  -> Tree.Lt
        AST.GtOp  -> Tree.Gt
        AST.LteOp -> Tree.Le
        AST.GteOp -> Tree.Ge
        _         -> error "Invalid operator for iRelOp"
    sRelOpExp op e1 e2 = do
      params <- liftA2 (\a b -> [a, b]) (unEx e1) (unEx e2)
      case op of
        AST.EqOp -> fmap Cx $ CJumpStm Ne
          <$> F.externalCall f_ "stringEqual" params <*> pure (ConstExp 0)
        AST.NeqOp -> fmap Cx $ CJumpStm Eq
          <$> F.externalCall f_ "stringEqual" params <*> pure (ConstExp 0)
        AST.LtOp -> fmap Cx $ CJumpStm Lt
          <$> F.externalCall f_ "stringCompare" params <*> pure (ConstExp 0)
        AST.GtOp -> fmap Cx $ CJumpStm Gt
          <$> F.externalCall f_ "stringCompare" params <*> pure (ConstExp 0)
        AST.LteOp -> fmap Cx $ CJumpStm Le
          <$> F.externalCall f_ "stringCompare" params <*> pure (ConstExp 0)
        AST.GteOp -> fmap Cx $ CJumpStm Ge
          <$> F.externalCall f_ "stringCompare" params <*> pure (ConstExp 0)
        _ -> error "Invalid operator for sRelOp"
    ifElseExp e1 e2 Nothing = do
      e2' <- unNx e2
      t <- newLabel
      f <- newLabel
      let body = stmSeq [unCx e1 t f, LabelStm t, e2', LabelStm f]
      pure $ Ex $ ESeqExp body (ConstExp 0)
    ifElseExp e1 e2 (Just e3) = do
      e2' <- unEx e2
      e3' <- unEx e3
      temp <- newTemp
      t <- newLabel
      f <- newLabel
      joinLabel <- newLabel
      let body = stmSeq
            [ unCx e1 t f
            , LabelStm t
            , MoveStm (TempExp temp) e2'
            , JumpStm (NameExp joinLabel) [joinLabel]
            , LabelStm f
            , MoveStm (TempExp temp) e3'
            , LabelStm joinLabel
            ]
      pure $ Ex $ ESeqExp body (TempExp temp)
    whileExp test body done = do
      start <- newLabel
      t <- newLabel
      test' <- unEx test
      body' <- unNx body
      pure $ Nx $ stmSeq
        [ LabelStm start
        , CJumpStm Eq test' (ConstExp 0) done t
        , LabelStm t
        , body'
        , JumpStm (NameExp start) [start]
        , LabelStm done
        ]
    breakExp l = pure $ Nx $ JumpStm (NameExp l) [l]
    funCallExp Outermost _ name exps =
      Ex . CallExp (NameExp name) <$> traverse unEx exps
    funCallExp level levelCall name exps =
      Ex . CallExp (NameExp name) . (staticLinks levelCall (levelParent level) :)
        <$> traverse unEx exps
    letExp stms exp = do
      exp' <- unEx exp
      Ex <$> case stms of
        []    -> pure exp'
        [stm] -> liftA2 ESeqExp (unNx stm) (pure exp')
        _     -> liftA2 ESeqExp (stmSeq <$> traverse unNx stms) (pure exp')
    seqExp [] = pure $ Ex $ ConstExp 0
    seqExp [exp] = Ex <$> unEx exp
    seqExp exps = Ex <$>
      liftA2 ESeqExp (stmSeq <$> traverse unNx (init exps)) (unEx (last exps))
    assignExp left right = Nx <$> liftA2 MoveStm (unEx left) (unEx right)

    functionDec level body = put . flip ProcFrag frame =<<
      F.procEntryExit1 f_ frame . MoveStm (TempExp (F.rv frame)) =<< unEx body
     where frame = levelFrame level
  in Translate_{..}