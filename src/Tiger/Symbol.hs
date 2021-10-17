{-# LANGUAGE LambdaCase #-}
module Tiger.Symbol
  ( Symbol ()
  , Gen
  , MonadSymbol (..)
  , mkSymbolGen
  , symbolId
  ) where

import Tiger.IntVar (newIntVar, readIntVar, writeIntVar)
import qualified Data.HashTable.IO as H

newtype Symbol = Symbol (String, Int)
instance Show Symbol where
  show (Symbol (s, _)) = s
instance Eq Symbol where
  Symbol (_, s1) == Symbol (_, s2) = s1 == s2
instance Ord Symbol where
  compare (Symbol (_, s1)) (Symbol (_, s2)) = compare s1 s2

type Gen = String -> IO Symbol

mkSymbolGen :: IO Gen
mkSymbolGen = do
  nextSym <- newIntVar 0
  table <- H.new :: IO (H.BasicHashTable String Int)
  let go s = H.lookup table s >>= \case
        Just i  -> return $ Symbol (s, i)
        Nothing -> do
          i <- readIntVar nextSym
          writeIntVar nextSym (i + 1)
          H.insert table s i
          return $ Symbol (s, i)
  return go

symbolId :: Symbol -> Int
symbolId (Symbol (_, i)) = i

class Monad m => MonadSymbol m where
  symbol :: String -> m Symbol