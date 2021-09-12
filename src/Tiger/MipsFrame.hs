{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Tiger.MipsFrame where

import Tiger.Temp
import qualified Tiger.Frame as F
import qualified Tiger.Translate as T

data MipsAccess = InFrame Int | InReg Temp
data MipsFrame
data MipsLevel

instance F.Frame MipsFrame where
  type Access MipsFrame = MipsAccess
  name = undefined
  formals = undefined

instance T.Translate MipsLevel where
  type Frame MipsLevel = MipsFrame
  outermost = undefined
  formals = undefined

newtype WithMips m a = WithMips (m a)
  deriving (Functor, Applicative, Monad)

instance MonadTemp m => F.MonadFrame (WithMips m) where
  type Frame' (WithMips m) = MipsFrame
  newFrame = undefined
  allocLocal = undefined

instance MonadTemp m => T.MonadTranslate (WithMips m) where
  type Level (WithMips m) = MipsLevel
  newLevel = undefined
  allocLocal = undefined