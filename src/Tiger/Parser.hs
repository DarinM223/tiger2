{-# LANGUAGE DerivingVia #-}
module Tiger.Parser (Parser (), runParser, symbol) where

import Control.Monad.Reader (ReaderT (ReaderT))
import Tiger.Symbol (Gen, MonadSymbol (symbol))

newtype Parser a = Parser { runParser :: Gen -> IO a }
  deriving (Functor, Applicative, Monad) via ReaderT Gen IO

instance MonadSymbol Parser where
  symbol s = Parser ($ s)