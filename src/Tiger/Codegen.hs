{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Tiger.Codegen where

import Tiger.Frame (Frame)
import Tiger.Temp (Label, Temp)
import Tiger.Tree (Stm)

data Instr
  = OperInstr String [Temp] [Temp] (Maybe [Label])
  | LabelInstr String
  | MoveInstr String Temp Temp

class (Monad m, Frame (Frame' m)) => MonadCodegen m where
  type Frame' m

  codegen :: Frame' m -> Stm -> m [Instr]