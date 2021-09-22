{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
module Tiger.Translate where

import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Tiger.Temp
import Tiger.Tree hiding (Exp)
import qualified Tiger.AST as AST
import qualified Tiger.Frame as F
import qualified Tiger.Tree as Tree

data Exp = Ex Tree.Exp
         | Nx Tree.Stm
         | Cx (Label -> Label -> Tree.Stm)

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

unEx :: MonadTemp m => Exp -> m Tree.Exp
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

unNx :: MonadTemp m => Exp -> m Tree.Stm
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

type Access level = (level, F.Access (Frame level))

class MonadPut w m | m -> w where
  put :: w -> m ()

class Translate level where
  type Frame level
  outermost :: level
  formals   :: level -> [Access level]
  simpleVar :: Access level -> level -> Exp

class (Monad m, Translate (Level m)) => MonadTranslate m where
  type Level m
  newLevel     :: Level m -> Label -> [Bool] -> m (Level m)
  allocLocal   :: Level m -> Bool -> m (Access (Level m))
  subscriptVar :: Exp -> Exp -> m Exp
  fieldVar     :: Exp -> Label -> [Label] -> m Exp
  intExp       :: Int -> m Exp
  stringExp    :: String -> m Exp
  binOp        :: AST.Op -> Exp -> Exp -> m Exp
  iRelOp       :: AST.Op -> Exp -> Exp -> m Exp
  sRelOp       :: AST.Op -> Exp -> Exp -> m Exp
  ifElseExp    :: Exp -> Exp -> Maybe Exp -> m Exp

data Frag frame = ProcFrag Stm frame
                | StringFrag Label String

data MipsLevel frame
  = Outermost
  | Level
  { levelParent  :: MipsLevel frame
  , levelFrame   :: Frame (MipsLevel frame)
  , levelFormals :: [Access (MipsLevel frame)]
  , levelUnique  :: Unique
  }

instance Eq (MipsLevel frame) where
  Outermost == Outermost = True
  Outermost == _ = False
  _ == Outermost = False
  l1 == l2 = levelUnique l1 == levelUnique l2

instance F.Frame frame => Translate (MipsLevel frame) where
  type Frame (MipsLevel frame) = frame
  outermost = Outermost
  formals Outermost = []
  formals l = tail (levelFormals l)
  simpleVar (lg, access) lf = Ex $ go (TempExp (F.fp (levelFrame lf))) lf
   where
    go build lf' | lg == lf' = F.exp access build
    go build lf' = go (F.exp staticLink build) (levelParent lf')
     where (_, staticLink) = head $ levelFormals lf'

newtype WithFrame frame m a = WithFrame (m a)
  deriving (Functor, Applicative, Monad)

instance
  ( MonadTemp m, MonadUnique m
  , MonadPut (Frag frame) m
  , F.MonadFrame m, F.Frame' m ~ frame
  ) => MonadTranslate (WithFrame frame m) where
  type Level (WithFrame frame m) = MipsLevel frame
  newLevel parent name escapes = WithFrame $ do
    frame <- F.newFrame name (True:escapes)
    u <- unique
    let level = Level parent frame ((level ,) <$> F.formals frame) u
    pure level
  allocLocal Outermost _ = error "Calling allocLocal on an Outermost level"
  allocLocal level escapes = WithFrame $
    (level ,) <$> F.allocLocal (levelFrame level) escapes
  subscriptVar e i = WithFrame $ do
    e' <- unEx e
    i' <- unEx i
    pure $ Ex $ MemExp $ BinOpExp Plus e' $
      BinOpExp Mul i' $ ConstExp (F.wordSize @frame)
  fieldVar e f fs = WithFrame $ do
    e' <- unEx e
    pure $ Ex $ MemExp $ BinOpExp Plus e' $
      BinOpExp Mul i $ ConstExp (F.wordSize @frame)
   where i = ConstExp $ fromJust $ elemIndex f fs
  intExp = pure . Ex . ConstExp
  stringExp lit = WithFrame $ do
    label <- newLabel
    put $ StringFrag label lit
    pure $ Ex $ NameExp label
  binOp op e1 e2 = WithFrame $ fmap Ex $ BinOpExp op' <$> unEx e1 <*> unEx e2
   where
    op' = case op of
      AST.AddOp -> Tree.Plus
      AST.SubOp -> Tree.Minus
      AST.MulOp -> Tree.Mul
      AST.DivOp -> Tree.Div
      _         -> error "Invalid operator for binOp"
  iRelOp op e1 e2 = WithFrame $ fmap Cx $ CJumpStm op' <$> unEx e1 <*> unEx e2
   where
    op' = case op of
      AST.EqOp  -> Tree.Eq
      AST.NeqOp -> Tree.Ne
      AST.LtOp  -> Tree.Lt
      AST.GtOp  -> Tree.Gt
      AST.LteOp -> Tree.Le
      AST.GteOp -> Tree.Ge
      _         -> error "Invalid operator for iRelOp"
  sRelOp op e1 e2 = WithFrame $ do
    params <- (\a b -> [a, b]) <$> unEx e1 <*> unEx e2
    case op of
      AST.EqOp -> fmap Cx $ CJumpStm Ne
        <$> F.externalCall "stringEqual" params <*> pure (ConstExp 0)
      AST.NeqOp -> fmap Cx $ CJumpStm Eq
        <$> F.externalCall "stringEqual" params <*> pure (ConstExp 0)
      _ -> error "Invalid operator for sRelOp"
  ifElseExp e1 e2 Nothing = WithFrame $ do
    e2' <- unNx e2
    t <- newLabel
    f <- newLabel
    let body = stmSeq [unCx e1 t f, LabelStm t, e2', LabelStm f]
    pure $ Ex $ ESeqExp body (ConstExp 0)
  ifElseExp e1 e2 (Just e3) = WithFrame $ do
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