module Tiger.Temp where

import Tiger.Symbol (Symbol, MonadSymbol)
import qualified Data.Unique as Unique

newtype Temp = Temp Int deriving (Eq, Show)
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