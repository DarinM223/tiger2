{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedLabels #-}
module Tiger.Symbol
  ( Symbol (Symbol)
  , SymbolState
  , SymGen
  , MonadSymbol (symbol)
  , mkSymbolGen
  , symbolId
  ) where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import Tiger.IntVar (newIntVar, readIntVar, writeIntVar)
import qualified Data.HashTable.IO as H
import qualified Data.HashMap.Strict as HM
import Control.Monad.State.Strict (StateT)
import Optics
import Optics.State.Operators

newtype Symbol = Symbol (String, Int)
  deriving Generic
instance Show Symbol where
  show (Symbol (s, _)) = s
instance Eq Symbol where
  Symbol (_, s1) == Symbol (_, s2) = s1 == s2
instance Ord Symbol where
  compare (Symbol (_, s1)) (Symbol (_, s2)) = compare s1 s2
instance NFData Symbol

class MonadSymbol m where
  symbol :: String -> m Symbol

data SymbolState = SymbolState
  { counter :: {-# UNPACK #-} !Int
  , table   :: {-# UNPACK #-} !(HM.HashMap String Int)
  } deriving Generic

instance Monad m => MonadSymbol (StateT SymbolState m) where
  symbol s = guse (#table % at' s) >>= \case
    Just i  -> return $! Symbol (s, i)
    Nothing -> do
      i <- guse #counter
      #counter %= (+ 1)
      #table % at' s ?= i
      return $! Symbol (s, i)

type SymGen = String -> IO Symbol

mkSymbolGen :: IO SymGen
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