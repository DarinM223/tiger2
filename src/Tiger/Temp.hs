{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Tiger.Temp where

import Control.DeepSeq (NFData)
import System.IO.Unsafe (unsafeInterleaveIO)
import Tiger.IntVar (newIntVar, readIntVar, writeIntVar)
import Tiger.Symbol (Symbol)
import qualified Data.Unique as Unique

newtype Temp = Temp { unTemp :: Int }
  deriving (Eq, NFData)
instance Show Temp where
  show (Temp i) = show i

mkTempGen :: Int -> IO (IO Temp)
mkTempGen initial = do
  ref <- newIntVar initial
  let go = do
        t <- readIntVar ref
        writeIntVar ref (t + 1)
        return $ Temp t
  return go

label :: Monad m => (String -> m Symbol) -> m Temp -> m Label
label sym temp = temp >>= sym . ("L" ++) . show

data Supply a = S a (Supply a) (Supply a)
  deriving (Functor, Foldable)

instance Applicative Supply where
  pure a = S a (pure a) (pure a)
  S f l1 r1 <*> S a l2 r2 = S (f a) (l1 <*> l2) (r1 <*> r2)

mkSupply :: IO a -> IO (Supply a)
mkSupply gen = go
 where
  go = unsafeInterleaveIO $ S <$> unsafeInterleaveIO gen <*> go <*> go

supplies :: Supply a -> [Supply a]
supplies (S _ s1 s2) = s1:supplies s2

type Label = Symbol

data Temp_ m = Temp_
  { newTemp    :: m Temp
  , newLabel   :: m Label
  , namedLabel :: String -> m Label
  }

temp_ :: Monad m => (String -> m Symbol) -> m Temp -> Temp_ m
temp_ symbol temp = Temp_ temp (label symbol temp) symbol

newtype Unique = Unique Unique.Unique
  deriving Eq

instance Show Unique where
  show (Unique _) = "Unique"

newUnique :: IO Unique
newUnique = Unique <$> Unique.newUnique