{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Tiger.MipsFrame where

import Control.Monad.IO.Class (MonadIO (..))
import Tiger.IntVar (IntVar, readIntVar, writeIntVar, newIntVar)
import Tiger.Temp (Label, MonadTemp (namedLabel, newTemp), Temp)
import Tiger.Tree
import qualified Tiger.Frame as F

data MipsFrame = MipsFrame
  { frameName    :: Label
  , frameLocals  :: IntVar
  , frameFormals :: [F.Access MipsFrame]
  , frameFp      :: Temp
  , frameRv      :: Temp
  }
instance F.Frame MipsFrame where
  data Access MipsFrame = InFrame Int | InReg Temp
  name = frameName
  formals = frameFormals
  fp = frameFp
  rv = frameRv
  wordSize = 4
  exp (InFrame k) temp = MemExp $ BinOpExp Plus temp (ConstExp k)
  exp (InReg t) _ = TempExp t

newtype Mips m a = Mips (m a)
  deriving (Functor, Applicative, Monad)

instance (MonadIO m, MonadTemp m) => F.MonadFrame (Mips m) where
  type Frame' (Mips m) = MipsFrame
  newFrame name escapes = Mips $ MipsFrame name
    <$> liftIO (newIntVar 0) <*> go escapes 0 <*> newTemp <*> newTemp
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