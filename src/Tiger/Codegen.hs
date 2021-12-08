module Tiger.Codegen where

import Tiger.Temp (Label, Temp)

type Dest = Temp
type Src = Temp
type Jump = Label

data Instr
  = OperInstr String [Src] [Dest] (Maybe [Jump])
  | LabelInstr String
  | MoveInstr String Src Dest