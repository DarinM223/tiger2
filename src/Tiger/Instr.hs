module Tiger.Instr where

import Tiger.Temp (Label, Temp)

type Dest = Temp
type Src = Temp
type Jump = Label

data Instr
  = OperInstr String [Src] [Dest] (Maybe [Jump])
  | LabelInstr String Label
  | MoveInstr String Src Dest
  deriving Show