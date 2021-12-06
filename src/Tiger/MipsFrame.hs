{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Tiger.MipsFrame where

import Control.Monad (replicateM)
import Control.Monad.IO.Class (MonadIO (..))
import Tiger.IntVar (IntVar, readIntVar, writeIntVar, newIntVar)
import Tiger.Temp (Label, MonadTemp (namedLabel, newTemp), Temp)
import Tiger.Tree
import qualified Tiger.Frame as F

data MipsFrame = MipsFrame
  { frameName        :: Label
  , frameLocals      :: IntVar
  , frameFormals     :: [F.Access MipsFrame]
  , frameFp          :: Temp
  , frameSp          :: Temp
  , frameRv          :: Temp
  , frameRa          :: Temp
  , frameArgRegs     :: [Temp]
  , frameCallerSaves :: [Temp]
  , frameCalleeSaves :: [Temp]
  }

instance Show (F.Access MipsFrame) => Show MipsFrame where
  show = show . frameName

instance F.Frame MipsFrame where
  data Access MipsFrame = InFrame Int | InReg Temp deriving Show
  name = frameName
  formals = frameFormals
  fp = frameFp
  sp = frameSp
  rv = frameRv
  ra = frameRa
  specialRegs f = ($ f) <$> [F.fp, F.sp, F.rv, F.ra]
  argRegs = frameArgRegs
  callerSaves = frameCallerSaves
  calleeSaves = frameCalleeSaves
  wordSize = 4
  exp (InFrame k) temp = MemExp $ BinOpExp Plus temp (ConstExp k)
  exp (InReg t) _ = TempExp t

newtype Mips m a = Mips (m a)
  deriving (Functor, Applicative, Monad)

instance (MonadIO m, MonadTemp m) => F.MonadFrame (Mips m) where
  type Frame' (Mips m) = MipsFrame
  newFrame frameName escapes = Mips $ do
    frameLocals <- liftIO $ newIntVar 0
    frameFormals <- go escapes 0
    frameFp <- newTemp
    frameSp <- newTemp
    frameRv <- newTemp
    frameRa <- newTemp
    frameArgRegs <- replicateM 4 newTemp
    frameCallerSaves <- replicateM 8 newTemp
    frameCalleeSaves <- replicateM 8 newTemp
    return MipsFrame{..}
   where
    go [] _ = pure []
    go (True:es) offset =
      (InFrame offset :) <$> go es (offset + F.wordSize @MipsFrame)
    go (False:es) offset = newTemp >>= \t -> (InReg t :) <$> go es offset
  allocLocal frame True = Mips $ liftIO $ do
    locals <- readIntVar $ frameLocals frame
    let offset = (locals + 1) * F.wordSize @MipsFrame
    writeIntVar (frameLocals frame) (locals + 1)
    pure $ InFrame offset
  allocLocal _ False = Mips (InReg <$> newTemp)
  externalCall s args = Mips $
    fmap (\label -> CallExp (NameExp label) args) (namedLabel s)
  procEntryExit1 _ body = pure body