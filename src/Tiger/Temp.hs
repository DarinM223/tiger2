module Tiger.Temp where

import Tiger.Symbol (Symbol, MonadSymbol)

newtype Temp = Temp Int deriving (Eq, Show)
type Label = Symbol

class MonadSymbol m => MonadTemp m where
  newTemp    :: m Temp
  newLabel   :: m Label
  namedLabel :: String -> m Label