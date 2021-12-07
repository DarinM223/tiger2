{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Tiger.MipsFrame where

import Control.Monad (replicateM)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (ask))
import Tiger.IntVar (IntVar, readIntVar, writeIntVar, newIntVar)
import Tiger.Temp (Label, MonadTemp (namedLabel, newTemp), Temp)
import Tiger.Tree
import qualified Tiger.Frame as F

data MipsRegisters = MipsRegisters
  { regFp          :: Temp
  , regSp          :: Temp
  , regRv          :: Temp
  , regRa          :: Temp
  , regArgRegs     :: [Temp]
  , regCallerSaves :: [Temp]
  , regCalleeSaves :: [Temp]
  }

mkMipsRegisters :: IO Temp -> IO MipsRegisters
mkMipsRegisters temp = do
  regFp <- temp
  regSp <- temp
  regRv <- temp
  regRa <- temp
  regArgRegs <- replicateM 4 temp
  regCallerSaves <- replicateM 8 temp
  regCalleeSaves <- replicateM 8 temp
  return MipsRegisters{..}

data MipsFrame = MipsFrame
  { frameName      :: Label
  , frameLocals    :: IntVar
  , frameFormals   :: [F.Access MipsFrame]
  , frameRegisters :: MipsRegisters
  }

instance Show (F.Access MipsFrame) => Show MipsFrame where
  show = show . frameName

instance F.Frame MipsFrame where
  data Access MipsFrame = InFrame Int | InReg Temp deriving Show
  name = frameName
  formals = frameFormals
  fp = regFp . frameRegisters
  sp = regSp . frameRegisters
  rv = regRv . frameRegisters
  ra = regRa . frameRegisters
  specialRegs f = ($ f) <$> [F.fp, F.sp, F.rv, F.ra]
  argRegs = regArgRegs . frameRegisters
  callerSaves = regCallerSaves . frameRegisters
  calleeSaves = regCalleeSaves . frameRegisters
  wordSize = 4
  exp (InFrame k) temp = MemExp $ BinOpExp Plus temp (ConstExp k)
  exp (InReg t) _ = TempExp t

newtype Mips m a = Mips (m a)
  deriving (Functor, Applicative, Monad)

instance (MonadIO m, MonadTemp m, MonadReader MipsRegisters m)
  => F.MonadFrame (Mips m) where
  type Frame' (Mips m) = MipsFrame
  newFrame name escapes = Mips $
    MipsFrame name <$> liftIO (newIntVar 0) <*> go escapes 0 <*> ask
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