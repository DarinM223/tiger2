{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Tiger.Frame where

import Tiger.Temp (Label, Temp)
import qualified Tiger.Tree as Tree

class Frame frame where
  data Access frame
  name     :: frame -> Label
  formals  :: frame -> [Access frame]
  fp       :: frame -> Temp
  rv       :: frame -> Temp
  wordSize :: Int
  exp      :: Access frame -> Tree.Exp -> Tree.Exp

class (Monad m, Frame (Frame' m)) => MonadFrame m where
  type Frame' m
  newFrame       :: Label -> [Bool] -> m (Frame' m)
  allocLocal     :: Frame' m -> Bool -> m (Access (Frame' m))
  externalCall   :: String -> [Tree.Exp] -> m Tree.Exp
  procEntryExit1 :: Frame' m -> Tree.Stm -> m Tree.Stm