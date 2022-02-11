{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
module Tiger.Symbol
  ( Symbol (Symbol)
  , SymGen
  , mkSymbolGen
  , symbolId
  ) where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import Tiger.IntVar (newIntVar, readIntVar, writeIntVar)
import qualified Data.HashTable.IO as H

newtype Symbol = Symbol (String, Int)
  deriving Generic
instance Show Symbol where
  show (Symbol (s, _)) = s
instance Eq Symbol where
  Symbol (_, s1) == Symbol (_, s2) = s1 == s2
instance Ord Symbol where
  compare (Symbol (_, s1)) (Symbol (_, s2)) = compare s1 s2
instance NFData Symbol

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