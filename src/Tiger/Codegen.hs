module Tiger.Codegen where

import Tiger.Assem (Instr)
import Tiger.MipsFrame (MipsFrame)
import Tiger.Temp (Supply, Temp)
import Tiger.Tree (Stm)
import qualified Tiger.MipsGen as MipsGen

class Codegen frame where
  codegen :: Supply Temp -> frame -> Stm -> [Instr]

instance Codegen MipsFrame where
  codegen = MipsGen.codegen