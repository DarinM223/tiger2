module Tiger.AST where

import Tiger.Tokens (AlexPosn)

data Symbol = Symbol deriving (Show, Eq)

type Pos = AlexPosn

data Dec = TyDec Pos Symbol Ty
         | VarDec Pos Symbol (Maybe Symbol) Exp
         | FunDec Pos Symbol [TyField] (Maybe Symbol) Exp
         deriving (Show, Eq)

data Ty = IdTy Pos Symbol
        | FieldsTy [TyField]
        | ArrayOfTy Pos Symbol
        deriving (Show, Eq)

data TyField = TyField Pos Symbol Symbol
  deriving (Show, Eq)

data Exp
  = VarExp Var
  | NilExp Pos
  | SeqExp [Exp]
  | NoValExp Pos
  | IntExp Pos Int
  | StringExp Pos String
  | OpExp Pos Op Exp Exp
  | FuncallExp Pos Symbol [Exp]
  | RecordExp Pos Symbol [(Pos, Symbol, Exp)]
  | ArrayExp Pos Symbol Exp Exp
  | AssignExp Pos Var Exp
  | IfExp Pos Exp Exp (Maybe Exp)
  | WhileExp Pos Exp Exp
  | ForExp Pos Symbol Exp Exp Exp
  | BreakExp Pos
  | LetExp Pos [Dec] Exp
  deriving (Show, Eq)

data Var = Var Pos Symbol
         | RecField Pos Var Symbol
         | ArraySub Pos Var Exp
         deriving (Show, Eq)

data Op = AddOp | SubOp | MulOp | DivOp
        | EqOp | NeqOp | GtOp | LtOp | GteOp | LteOp
        | AndOp | OrOp
        deriving (Show, Eq)
