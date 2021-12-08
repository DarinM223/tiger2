{-# LANGUAGE DerivingVia #-}
module Tiger.Parser (Parser (), runParser, symbol) where

import Control.Monad.Reader (ReaderT (ReaderT))
import Tiger.Symbol (MonadSymbol (symbol), SymGen)

newtype Parser a = Parser { runParser :: SymGen -> IO a }
  deriving (Functor, Applicative, Monad) via ReaderT SymGen IO

instance MonadSymbol Parser where
  symbol s = Parser ($ s)