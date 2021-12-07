{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Tiger.Tc
  ( MonadTc
  , MonadUnique (..)
  , MonadCheck (..)
  , runTc
  , symbol
  ) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT (ReaderT), MonadReader (ask, local))
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import System.IO (hPutStrLn, stderr)
import Tiger.Frame (MonadFrame)
import Tiger.IntVar (newIntVar, readIntVar, writeIntVar, IntVar)
import Tiger.MipsFrame (Mips (..), MipsFrame, MipsRegisters)
import Tiger.Symbol (Gen, MonadSymbol (symbol))
import Tiger.Temp
import Tiger.Translate
  (Frag, Level, MonadPut (put), MonadTranslate, WithFrame (..))

-- TODO(DarinM223): move to seperate monads and keep Tc clean

data TcState = TcState
  { compilationFailed :: IntVar
  , _symGen           :: Gen
  , _tmpGen           :: IO Temp
  , fragList          :: IORef [Frag MipsFrame]
  , registers         :: MipsRegisters
  }

newtype Tc a = Tc (TcState -> IO a)
  deriving (Functor, Applicative, Monad, MonadIO) via ReaderT TcState IO
  deriving (MonadSymbol, MonadTemp) via FromRecord "_symGen" "_tmpGen" TcState
  deriving MonadFrame via Mips Tc
  deriving (MonadTranslate (Level MipsFrame)) via WithFrame MipsFrame Tc

runTc
  :: Gen -> IO Temp -> MipsRegisters -> Tc a
  -> IO (Either () (a, [Frag MipsFrame]))
runTc symGen tmpGen regs (Tc f) = do
  var <- newIntVar 0
  fragListRef <- newIORef []
  r <- f (TcState var symGen tmpGen fragListRef regs)
  l <- readIORef fragListRef
  (\failed -> if failed == 0 then Right (r, l) else Left ()) <$> readIntVar var

instance MonadReader MipsRegisters Tc where
  ask = Tc $ pure . registers
  local f (Tc m) = Tc $ \r -> m r{registers = f (registers r)}

instance MonadPut (Frag MipsFrame) Tc where
  put f = Tc $ flip modifyIORef' (f :) . fragList

instance MonadUnique Tc where
  unique = Tc $ const newUnique

class Monad m => MonadCheck m where
  compileError :: String -> m ()

instance MonadCheck Tc where
  compileError err = Tc $ \TcState{compilationFailed = var} ->
    writeIntVar var 1 >> hPutStrLn stderr err

type MonadTc l m =
  (MonadTemp m, MonadUnique m, MonadCheck m, MonadTranslate l m)