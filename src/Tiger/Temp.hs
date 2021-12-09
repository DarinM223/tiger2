{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
module Tiger.Temp where

import Control.Monad.Reader (ReaderT (ReaderT))
import GHC.Records (HasField (..))
import System.IO.Unsafe (unsafeInterleaveIO)
import Tiger.IntVar (newIntVar, readIntVar, writeIntVar)
import Tiger.Symbol (MonadSymbol, Symbol, SymGen, symbol)
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
  deriving Functor

instance Applicative Supply where
  pure a = S a (pure a) (pure a)
  S f l1 r1 <*> S a l2 r2 = S (f a) (l1 <*> l2) (r1 <*> r2)

mkSupply :: IO a -> IO (Supply a)
mkSupply gen = go
 where
  go = unsafeInterleaveIO $ S <$> unsafeInterleaveIO gen <*> go <*> go

type Label = Symbol

class MonadSymbol m => MonadTemp m where
  newTemp    :: m Temp
  newLabel   :: m Label
  namedLabel :: String -> m Label

newtype FromRecord (sym :: Lits.Symbol) (tmp :: Lits.Symbol) r a
  = FromRecord (r -> IO a)
  deriving (Functor, Applicative, Monad) via ReaderT r IO

instance HasField sym r SymGen => MonadSymbol (FromRecord sym tmp r) where
  symbol s = FromRecord $ ($ s) . getField @sym

instance (HasField sym r SymGen, HasField tmp r (IO Temp))
  => MonadTemp (FromRecord sym tmp r) where
  newTemp = FromRecord $ getField @tmp
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