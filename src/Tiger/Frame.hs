{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Tiger.Frame where

import Tiger.Temp (Label)

class Frame frame where
  data Access frame
  name    :: frame -> Label
  formals :: frame -> [Access frame]

class (Monad m, Frame (Frame' m)) => MonadFrame m where
  type Frame' m
  newFrame   :: Label -> [Bool] -> m (Frame' m)
  allocLocal :: Frame' m -> Bool -> m (Access (Frame' m), Frame' m)