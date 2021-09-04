module Tiger.AST where

import Tiger.Symbol (Symbol)
import Tiger.Tokens (AlexPosn)

type Pos = AlexPosn

data Dec = TyDec Pos Symbol Ty
         | VarDec Pos Symbol (Maybe Symbol) Exp
         | FunDec Pos Symbol [TyField] (Maybe Symbol) Exp
         deriving (Show, Eq)

data Ty = IdTy Symbol
        | FieldsTy Pos [TyField]
        | ArrayOfTy Pos Symbol
        deriving (Show, Eq)

data TyField = TyField Pos Symbol Symbol
  deriving (Show, Eq)

data Exp
  = VarExp Var
  | NilExp Pos
  | SeqExp Pos [Exp]
  | IntExp Int
  | StringExp String
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

data Var = Var Symbol
         | RecField Pos Var Symbol
         | ArraySub Pos Var Exp
         deriving (Show, Eq)

data Op = AddOp | SubOp | MulOp | DivOp
        | EqOp | NeqOp | GtOp | LtOp | GteOp | LteOp
        | AndOp | OrOp
        deriving (Show, Eq)
