module Tiger.Temp where

import System.IO.Unsafe (unsafeInterleaveIO)
import Tiger.IntVar (newIntVar, readIntVar, writeIntVar)
import Tiger.Symbol (Symbol, MonadSymbol)
import qualified Data.Unique as Unique

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

data Supply = S Temp Supply Supply

mkSupply :: IO Temp -> IO Supply
mkSupply gen = go
 where
  go = unsafeInterleaveIO $ S <$> unsafeInterleaveIO gen <*> go <*> go

type Label = Symbol

class MonadSymbol m => MonadTemp m where
  newTemp    :: m Temp
  newLabel   :: m Label
  namedLabel :: String -> m Label

newtype Unique = Unique Unique.Unique
  deriving Eq

instance Show Unique where
  show (Unique _) = "Unique"

newUnique :: IO Unique
newUnique = Unique <$> Unique.newUnique

class Monad m => MonadUnique m where
  unique :: m Unique