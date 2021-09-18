{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
module Tiger.AST where

import Tiger.Symbol (Symbol)
import Tiger.Tokens (AlexPosn)

type Pos = AlexPosn

data Dec' r = TyDecs [TyDec r]
            | VarDec (VarDec' r)
            | FunDecs [FunDec r]
            deriving (Show, Eq, Functor, Foldable, Traversable)

data TyDec r = TyDec
  { typeDecPos  :: Pos
  , typeDecName :: Symbol
  , typeDecTy   :: Ty' r
  } deriving (Show, Eq, Functor, Foldable, Traversable)

data VarDec' r = VarDec'
  { varDecPos     :: Pos
  , varDecName    :: Symbol
  , varDecTy      :: Maybe Symbol
  , varDecInit    :: Exp' r
  , varDecEscapes :: r
  } deriving (Show, Eq, Functor, Foldable, Traversable)

data FunDec r = FunDec
  { funDecPos    :: Pos
  , funDecName   :: Symbol
  , funDecParams :: [TyField r]
  , funDecResult :: Maybe Symbol
  , funDecBody   :: Exp' r
  } deriving (Show, Eq, Functor, Foldable, Traversable)

data Ty' r = IdTy Symbol
           | FieldsTy Pos [TyField r]
           | ArrayOfTy Pos Symbol
           deriving (Show, Eq, Functor, Foldable, Traversable)

data TyField r = TyField Pos Symbol Symbol r
  deriving (Show, Eq, Functor, Foldable, Traversable)

data Exp' r
  = VarExp (Var' r)
  | NilExp Pos
  | SeqExp Pos [Exp' r]
  | IntExp Int
  | StringExp String
  | OpExp Pos Op (Exp' r) (Exp' r)
  | FuncallExp Pos Symbol [Exp' r]
  | RecordExp Pos Symbol [(Pos, Symbol, Exp' r)]
  | ArrayExp Pos Symbol (Exp' r) (Exp' r)
  | AssignExp Pos (Var' r) (Exp' r)
  | IfExp Pos (Exp' r) (Exp' r) (Maybe (Exp' r))
  | WhileExp Pos (Exp' r) (Exp' r)
  | ForExp Pos Symbol (Exp' r) (Exp' r) (Exp' r) r
  | BreakExp Pos
  | LetExp Pos [Dec' r] (Exp' r)
  deriving (Show, Eq, Functor, Foldable, Traversable)

data Var' r = Var Symbol
            | RecField Pos (Var' r) Symbol
            | ArraySub Pos (Var' r) (Exp' r)
            deriving (Show, Eq, Functor, Foldable, Traversable)

type Dec = Dec' Bool
type Exp = Exp' Bool
type Var = Var' Bool
type Ty = Ty' Bool

data Op = AddOp | SubOp | MulOp | DivOp
        | EqOp | NeqOp | GtOp | LtOp | GteOp | LteOp
        | AndOp | OrOp
        deriving (Show, Eq)