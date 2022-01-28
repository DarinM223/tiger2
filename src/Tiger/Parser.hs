{-# LANGUAGE DerivingVia #-}
module Tiger.Parser (Parser (), runParser, symbol) where

import Control.Monad.Reader (ReaderT (ReaderT))
import Tiger.Symbol (Symbol, SymGen)

newtype Parser a = Parser { runParser :: SymGen -> IO a }
  deriving (Functor, Applicative, Monad) via ReaderT SymGen IO

symbol :: String -> Parser Symbol
symbol s = Parser ($ s)