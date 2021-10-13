{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Tiger.Parser (Parser (), runParser, symbol) where

import Control.Monad.Reader
import Tiger.Symbol (Gen, MonadSymbol (symbol))

newtype Parser a = Parser (ReaderT Gen IO a)
  deriving (Functor, Applicative, Monad)

runParser :: Gen -> Parser a -> IO a
runParser f (Parser m) = runReaderT m f

instance MonadSymbol Parser where
  symbol s = Parser $ ask >>= lift . ($ s)