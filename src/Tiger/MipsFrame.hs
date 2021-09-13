{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
module Tiger.MipsFrame where

import Data.Functor ((<&>))
import Tiger.Temp
import qualified Tiger.Frame as F
import qualified Tiger.Translate as T

data MipsFrame = MipsFrame
  { frameName    :: Label
  , frameLocals  :: Int
  , frameFormals :: [F.Access MipsFrame]
  }
data MipsLevel
  = Outermost
  | Level
  { levelParent  :: MipsLevel
  , levelFrame   :: T.Frame MipsLevel
  , levelFormals :: [T.Access MipsLevel]
  }

instance F.Frame MipsFrame where
  data Access MipsFrame = InFrame Int | InReg Temp
  name = frameName
  formals = frameFormals

instance T.Translate MipsLevel where
  type Frame MipsLevel = MipsFrame
  outermost = Outermost
  formals Outermost = []
  formals l = levelFormals l

newtype WithMips m a = WithMips (m a)
  deriving (Functor, Applicative, Monad)

instance MonadTemp m => F.MonadFrame (WithMips m) where
  type Frame' (WithMips m) = MipsFrame
  newFrame name escapes =
    WithMips $ MipsFrame name 0 <$> go escapes 0
   where
    go [] _ = pure []
    go (True:es) offset = (InFrame offset :) <$> go es (offset + 4)
    go (False:es) offset = newTemp >>= \t -> (InReg t :) <$> go es offset
  allocLocal frame True = WithMips $ pure (InFrame offset, frame')
   where
    offset = (frameLocals frame + 1) * 4
    frame' = frame { frameLocals = frameLocals frame + 1 }
  allocLocal frame False = WithMips $ (InReg <$> newTemp) <&> (, frame)

instance (MonadTemp m, F.MonadFrame m, F.Frame' m ~ MipsFrame)
  => T.MonadTranslate (WithMips m) where
  type Level (WithMips m) = MipsLevel
  newLevel parent name escapes = WithMips $ do
    frame <- F.newFrame name (True:escapes)
    let level = Level parent frame ((level ,) <$> F.formals frame)
    pure level
  allocLocal Outermost _ = error "Calling allocLocal on an Outermost level"
  allocLocal level escapes = do
    (access, frame) <- F.allocLocal (levelFrame level) escapes
    pure ((level, access), level { levelFrame = frame })