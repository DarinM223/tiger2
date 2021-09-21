{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
module Tiger.Translate where

import Tiger.Temp
import Tiger.Tree hiding (Exp)
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

instance (MonadUnique m, F.MonadFrame m, F.Frame' m ~ frame)
  => MonadTranslate (WithFrame frame m) where
  type Level (WithFrame frame m) = MipsLevel frame
  newLevel parent name escapes = WithFrame $ do
    frame <- F.newFrame name (True:escapes)
    u <- unique
    let level = Level parent frame ((level ,) <$> F.formals frame) u
    pure level
  allocLocal Outermost _ = error "Calling allocLocal on an Outermost level"
  allocLocal level escapes = WithFrame $
    (level ,) <$> F.allocLocal (levelFrame level) escapes