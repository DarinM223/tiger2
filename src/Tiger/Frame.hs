{-# LANGUAGE TypeFamilies #-}
module Tiger.Frame where

import Tiger.Temp (Label)

class Frame frame where
  type Access frame
  name    :: frame -> Label
  formals :: frame -> [Access frame]

class Monad m => MonadFrame m where
  type Frame' m
  newFrame   :: Label -> [Bool] -> m (Frame' m)
  allocLocal :: frame -> Bool -> m (Access (Frame' m))