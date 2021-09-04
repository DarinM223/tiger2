{-# LANGUAGE LambdaCase #-}
module Tiger.Symbol (Symbol, Gen, symbolGen, symbolName) where

import Tiger.IntVar (newIntVar, readIntVar, writeIntVar)
import qualified Data.HashTable.IO as H

type Symbol = (String, Int)
type Gen = String -> IO Symbol

symbolGen :: IO Gen
symbolGen = do
  nextSym <- newIntVar 0
  table <- H.new :: IO (H.BasicHashTable String Int)
  let go s = H.lookup table s >>= \case
        Just i  -> return (s, i)
        Nothing -> do
          i <- readIntVar nextSym
          writeIntVar nextSym (i + 1)
          H.insert table s i
          return (s, i)
  return go

symbolName :: Symbol -> String
symbolName = fst