{-# LANGUAGE TypeFamilies #-}
module Tiger.Translate where

import Tiger.Temp (Label)
import qualified Tiger.Frame as F

type Access level = (level, F.Access (Frame level))

class Translate level where
  type Frame level
  outermost :: level
  formals   :: level -> [Access (Frame level)]

class Monad m => MonadTranslate m where
  type Level m
  newLevel   :: Level m -> Label -> [Bool] -> m (Level m)
  allocLocal :: Level m -> Bool -> m (Access (Frame (Level m)))