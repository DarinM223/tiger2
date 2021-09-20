{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Tiger.MipsFrame where

import Control.Monad.IO.Class (MonadIO (..))
import Tiger.IntVar (IntVar, readIntVar, writeIntVar, newIntVar)
import Tiger.Temp
import qualified Tiger.Frame as F
import qualified Tiger.Translate as T

data MipsFrame = MipsFrame
  { frameName    :: Label
  , frameLocals  :: IntVar
  , frameFormals :: [F.Access MipsFrame]
  , frameFp      :: Temp
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
  fp = frameFp
  wordSize = 4
  exp = undefined

instance T.Translate MipsLevel where
  type Frame MipsLevel = MipsFrame
  outermost = Outermost
  formals Outermost = []
  formals l = tail (levelFormals l)
  simpleVar = undefined

newtype WithMips m a = WithMips (m a)
  deriving (Functor, Applicative, Monad)

instance (MonadIO m, MonadTemp m) => F.MonadFrame (WithMips m) where
  type Frame' (WithMips m) = MipsFrame
  newFrame name escapes =
    WithMips $ MipsFrame name <$> liftIO (newIntVar 0) <*> go escapes 0 <*> newTemp
   where
    go [] _ = pure []
    go (True:es) offset = (InFrame offset :) <$> go es (offset + F.wordSize @MipsFrame)
    go (False:es) offset = newTemp >>= \t -> (InReg t :) <$> go es offset
  allocLocal frame True = WithMips $ liftIO $ do
    locals <- readIntVar $ frameLocals frame
    let offset = (locals + 1) * F.wordSize @MipsFrame
    writeIntVar (frameLocals frame) (locals + 1)
    pure $ InFrame offset
  allocLocal _ False = WithMips (InReg <$> newTemp)

instance (MonadIO m, MonadTemp m, F.MonadFrame m, F.Frame' m ~ MipsFrame)
  => T.MonadTranslate (WithMips m) where
  type Level (WithMips m) = MipsLevel
  newLevel parent name escapes = WithMips $ do
    frame <- F.newFrame name (True:escapes)
    let level = Level parent frame ((level ,) <$> F.formals frame)
    pure level
  allocLocal Outermost _ = error "Calling allocLocal on an Outermost level"
  allocLocal level escapes =
    (level ,) <$> F.allocLocal (levelFrame level) escapes