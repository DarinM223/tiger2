{-# LANGUAGE LambdaCase #-}
module Tiger.Assem where

import Data.Char (ord)
import Data.Maybe (fromMaybe)
import Tiger.Temp (Label, Temp)

type Dest = Temp
type Src = Temp
type Jump = Label

data Instr
  = OperInstr String [Src] [Dest] (Maybe [Jump])
  | LabelInstr String Label
  | MoveInstr String Src Dest
  deriving (Show, Eq)

format :: (Temp -> String) -> Instr -> String
format sayTemp = \case
  OperInstr assem srcs dests jmps -> '\t':speak assem srcs dests (fromMaybe [] jmps)
  MoveInstr assem src dest        -> '\t':speak assem [src] [dest] ([] :: [Jump])
  LabelInstr assem _              -> assem
 where
  speak assem srcs dests jmps = go assem
   where
    go ('`':'s':i:rest) = sayTemp (srcs !! (ord i - ord '0')) ++ go rest
    go ('`':'d':i:rest) = sayTemp (dests !! (ord i - ord '0')) ++ go rest
    go ('`':'j':i:rest) = show (jmps !! (ord i - ord '0')) ++ go rest
    go ('`':'`':rest)   = '`':go rest
    go ('`':_)          = error "Bad Assem format"
    go (c:rest)         = c:go rest
    go []               = []