{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Tiger.Codegen where

import Tiger.Frame (Frame)
import Tiger.Temp (Label, Temp)
import Tiger.Tree (Stm)

type Dest = Temp
type Src = Temp
type Jump = Label

data Instr
  = OperInstr String [Src] [Dest] (Maybe [Jump])
  | LabelInstr String
  | MoveInstr String Src Dest

class (Monad m, Frame (Frame' m)) => MonadCodegen m where
  type Frame' m

  codegen :: Frame' m -> Stm -> m [Instr]