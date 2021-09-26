{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
import Control.Monad.Reader (ReaderT (..), asks, MonadTrans (lift))
import Data.IORef (IORef, modifyIORef', newIORef)
import System.IO (hPutStrLn, stderr)
import Tiger.Frame (MonadFrame)
import Tiger.IntVar (newIntVar, readIntVar, writeIntVar, IntVar)
import Tiger.MipsFrame (Mips (..), MipsFrame)
import Tiger.Symbol (Gen, MonadSymbol (symbol))
import Tiger.Temp (MonadTemp (..), Temp (Temp), MonadUnique (unique), newUnique)
import Tiger.Translate
  (Frag, MonadPut (put), MipsLevel, MonadTranslate, WithFrame (..))

-- TODO(DarinM223): move to seperate monads and keep Tc clean

data TcState = TcState
  { compilationFailed :: IntVar
  , symbolGen         :: Gen
  , tempRef           :: IntVar
  , fragList          :: IORef [Frag MipsFrame]
  }

newtype Tc a = Tc (ReaderT TcState IO a)
  deriving (Functor, Applicative, Monad, MonadIO)
  deriving MonadFrame via Mips Tc
  deriving (MonadTranslate (MipsLevel MipsFrame)) via WithFrame MipsFrame Tc

runTc :: Gen -> Tc a -> IO (Either () a)
runTc gen (Tc m) = do
  var <- newIntVar 0
  ref <- newIntVar 0
  fragListRef <- newIORef []
  r <- runReaderT m (TcState var gen ref fragListRef)
  (\failed -> if failed == 0 then Right r else Left ()) <$> readIntVar var

instance MonadSymbol Tc where
  symbol s = Tc $ asks symbolGen >>= lift . ($ s)

instance MonadTemp Tc where
  newTemp = Tc $ do
    ref <- asks tempRef
    lift $ do
      t <- readIntVar ref
      writeIntVar ref (t + 1)
      return $ Temp t
  newLabel = newTemp >>= symbol . ("L" ++) . show
  namedLabel = symbol

instance MonadPut (Frag MipsFrame) Tc where
  put f = Tc $ asks fragList >>= lift . flip modifyIORef' (f :)

instance MonadUnique Tc where
  unique = Tc $ lift newUnique

class Monad m => MonadCheck m where
  compileError :: String -> m ()

instance MonadCheck Tc where
  compileError err = Tc $ do
    var <- asks compilationFailed
    lift $ writeIntVar var 1 >> hPutStrLn stderr err

type MonadTc l m =
  (MonadTemp m, MonadUnique m, MonadCheck m, MonadTranslate l m)