{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Tiger.Translate where

import Tiger.Temp (Label, MonadTemp (newLabel, newTemp))
import qualified Tiger.Frame as F
import qualified Tiger.Tree as Tree

type Access level = (level, F.Access (Frame level))

class Translate level where
  type Frame level
  outermost :: level
  formals   :: level -> [Access level]
  simpleVar :: Access level -> level -> Exp

class (Monad m, Translate (Level m)) => MonadTranslate m where
  type Level m
  newLevel   :: Level m -> Label -> [Bool] -> m (Level m)
  allocLocal :: Level m -> Bool -> m (Access (Level m))

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
unEx (Nx s) = pure $ Tree.ESeqExp s (Tree.ConstExp 0)
unEx (Cx genStm) = do
  r <- newTemp
  t <- newLabel
  f <- newLabel
  let branchTemp = Tree.stmSeq
        [ Tree.MoveStm (Tree.TempExp r) (Tree.ConstExp 1)
        , genStm t f
        , Tree.LabelStm f
        , Tree.MoveStm (Tree.TempExp r) (Tree.ConstExp 0)
        , Tree.LabelStm t
        ]
  pure $ Tree.ESeqExp branchTemp (Tree.TempExp r)

unNx :: MonadTemp m => Exp -> m Tree.Stm
unNx (Ex e) = pure $ Tree.ExpStm e
unNx (Nx s) = pure s
unNx (Cx genStm) = do
  t <- newLabel
  pure $ Tree.SeqStm (genStm t t) (Tree.LabelStm t)

unCx :: Exp -> Label -> Label -> Tree.Stm
unCx (Ex (Tree.ConstExp 0)) = \_ f -> Tree.JumpStm (Tree.NameExp f) [f]
unCx (Ex (Tree.ConstExp 1)) = \t _ -> Tree.JumpStm (Tree.NameExp t) [t]
unCx (Ex e) = flip (Tree.CJumpStm Tree.Eq e (Tree.ConstExp 0))
unCx (Cx genStm) = genStm
unCx (Nx _) = error "Calling unCx on an Nx constructor"