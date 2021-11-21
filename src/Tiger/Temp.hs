{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
module Tiger.Temp where

import Control.Monad.Reader (MonadIO (liftIO), MonadReader, asks)
import GHC.Records (HasField (..))
import System.IO.Unsafe (unsafeInterleaveIO)
import Tiger.IntVar (newIntVar, readIntVar, writeIntVar)
import Tiger.Symbol (Gen, MonadSymbol, Symbol, symbol)
import qualified Data.Unique as Unique
import qualified GHC.TypeLits as Lits

newtype Temp = Temp Int deriving Eq
instance Show Temp where
  show (Temp i) = show i

mkTempGen :: IO (IO Temp)
mkTempGen = do
  ref <- newIntVar 0
  let go = do
        t <- readIntVar ref
        writeIntVar ref (t + 1)
        return $ Temp t
  return go

label :: Monad m => (String -> m Symbol) -> m Temp -> m Label
label sym temp = temp >>= sym . ("L" ++) . show

data Supply a = S a (Supply a) (Supply a)

mkSupply :: IO a -> IO (Supply a)
mkSupply gen = go
 where
  go = unsafeInterleaveIO $ S <$> unsafeInterleaveIO gen <*> go <*> go

type Label = Symbol

class MonadSymbol m => MonadTemp m where
  newTemp    :: m Temp
  newLabel   :: m Label
  namedLabel :: String -> m Label

newtype SymbolFromField (sym :: Lits.Symbol) m a = SymbolFromField (m a)
  deriving (Functor, Applicative, Monad)

instance (MonadIO m, MonadReader r m, HasField sym r Gen)
  => MonadSymbol (SymbolFromField sym m) where
  symbol s = SymbolFromField $ asks (getField @sym) >>= liftIO . ($ s)

newtype TempFromField (sym :: Lits.Symbol) m a = TempFromField (m a)
  deriving (Functor, Applicative, Monad, MonadSymbol)

instance (MonadIO m, MonadReader r m, MonadSymbol m, HasField sym r (IO Temp))
  => MonadTemp (TempFromField sym m) where
  newTemp = TempFromField $ asks (getField @sym) >>= liftIO
  newLabel = label symbol newTemp
  namedLabel = symbol

newtype Unique = Unique Unique.Unique
  deriving Eq

instance Show Unique where
  show (Unique _) = "Unique"

newUnique :: IO Unique
newUnique = Unique <$> Unique.newUnique

class Monad m => MonadUnique m where
  unique :: m Unique