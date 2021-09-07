{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
module Tiger.Symbol
  ( pattern Sym
  , Symbol ()
  , Gen
  , symbolGen
  , symbolName
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

pattern Sym :: String -> Int -> Symbol
pattern Sym s i = Symbol (s, i)
{-# COMPLETE Sym #-}

type Gen = String -> IO Symbol

symbolGen :: IO Gen
symbolGen = do
  nextSym <- newIntVar 0
  table <- H.new :: IO (H.BasicHashTable String Int)
  let go s = H.lookup table s >>= \case
        Just i  -> return $ Sym s i
        Nothing -> do
          i <- readIntVar nextSym
          writeIntVar nextSym (i + 1)
          H.insert table s i
          return $ Sym s i
  return go

symbolName :: Symbol -> String
symbolName (Sym s _) = s