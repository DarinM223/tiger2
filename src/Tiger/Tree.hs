module Tiger.Tree where

import Tiger.Temp (Label, Temp)

data Exp = ConstExp Int
         | NameExp Label
         | TempExp Temp
         | BinOpExp BinOp Exp Exp
         | MemExp Exp
         | CallExp Exp [Exp]
         | ESeqExp Stm Exp
         deriving (Show, Eq)

data Stm = MoveStm Exp Exp
         | ExpStm Exp
         | JumpStm Exp [Label]
         | CJumpStm RelOp Exp Exp Label Label
         | SeqStm Stm Stm
         | LabelStm Label
         deriving (Show, Eq)

data BinOp = Plus | Minus | Mul | Div
           | And | Or | Lshift | Rshift | Arshift | Xor
           deriving (Show, Eq)

data RelOp = Eq | Ne | Lt | Gt | Le | Ge | Ult | Ule | Ugt | Uge
  deriving (Show, Eq)

stmSeq :: [Stm] -> Stm
stmSeq = foldr1 SeqStm