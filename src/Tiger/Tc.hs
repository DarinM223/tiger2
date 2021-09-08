{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Tiger.Tc
  ( Tc ()
  , runTc
  , symbol
  , unique
  , compileError
  ) where

import Control.Monad.Reader (ReaderT (..), asks, MonadTrans (lift))
import Tiger.IntVar (newIntVar, readIntVar, writeIntVar, IntVar)
import Tiger.Symbol (Gen, Symbol)
import Tiger.Types (Unique, newUnique)

data TcState = TcState
  { compilationFailed :: IntVar
  , symbolGen         :: Gen
  }

newtype Tc a = Tc (ReaderT TcState IO a)
  deriving (Functor, Applicative, Monad)

runTc :: Gen -> Tc a -> IO (Either () a)
runTc gen (Tc m) = do
  var <- newIntVar 0
  r <- runReaderT m (TcState var gen)
  (\failed -> if failed == 0 then Right r else Left ()) <$> readIntVar var

symbol :: String -> Tc Symbol
symbol s = Tc $ asks symbolGen >>= lift . ($ s)

unique :: Tc Unique
unique = Tc $ lift newUnique

compileError :: String -> Tc ()
compileError err = Tc $ do
  var <- asks compilationFailed
  lift $ writeIntVar var 1 >> putStrLn err