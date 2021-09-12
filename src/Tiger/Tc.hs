{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Tiger.Tc
  ( MonadTc
  , MonadUnique (..)
  , MonadCheck (..)
  , runTc
  , symbol
  ) where

import Control.Monad.Reader (ReaderT (..), asks, MonadTrans (lift))
import System.IO (hPutStrLn, stderr)
import Tiger.Frame (MonadFrame)
import Tiger.IntVar (newIntVar, readIntVar, writeIntVar, IntVar)
import Tiger.Symbol (Gen, MonadSymbol (symbol))
import Tiger.Temp (MonadTemp(..), Temp(Temp))
import Tiger.Translate (MonadTranslate)
import Tiger.Types (Unique, newUnique)

-- TODO(DarinM223): move to seperate monads and keep Tc clean
import Tiger.MipsFrame (WithMips (..))

data TcState = TcState
  { compilationFailed :: IntVar
  , symbolGen         :: Gen
  , tempRef           :: IntVar
  }

newtype Tc a = Tc (ReaderT TcState IO a)
  deriving (Functor, Applicative, Monad)
  deriving MonadFrame via WithMips Tc
  deriving MonadTranslate via WithMips Tc

runTc :: Gen -> Tc a -> IO (Either () a)
runTc gen (Tc m) = do
  var <- newIntVar 0
  ref <- newIntVar 0
  r <- runReaderT m (TcState var gen ref)
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

class Monad m => MonadUnique m where
  unique :: m Unique

instance MonadUnique Tc where
  unique = Tc $ lift newUnique

class Monad m => MonadCheck m where
  compileError :: String -> m ()

instance MonadCheck Tc where
  compileError err = Tc $ do
    var <- asks compilationFailed
    lift $ writeIntVar var 1 >> hPutStrLn stderr err

type MonadTc m =
  (MonadTemp m, MonadUnique m, MonadCheck m, MonadFrame m, MonadTranslate m)