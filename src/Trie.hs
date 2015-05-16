{-# LANGUAGE DeriveGeneric #-}

module Trie where

import           Control.DeepSeq
import           Data.List (foldl')
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           GHC.Generics

data Trie a
  = Trie Bool (Map a (Trie a))
  deriving (Show, Generic)

instance NFData a => NFData (Trie a)

pp :: Trie a -> String
pp = error "pp"

empty :: Trie a
empty = Trie False M.empty

insert :: Ord a => [a] -> Trie a -> Trie a
insert (a : r) (Trie b cs) = case M.lookup a cs of
  Nothing -> Trie b (M.insert a (insert r empty) cs)
  Just c -> Trie b (M.insert a (insert r c) cs)
insert [] (Trie _ cs) = Trie True cs

fromList :: Ord a => [[a]] -> Trie a
fromList = foldl' (flip insert) empty

toList :: Trie a -> [[a]]
toList (Trie b cs) =
  (if b then [[]] else []) ++
  concat (map (\ (c, t) -> map (c :) (toList t)) (M.toList cs))

member :: Ord a => [a] -> Trie a -> Bool
member [] (Trie b _) = b
member (a : r) (Trie _ cs) = case M.lookup a cs of
  Nothing -> False
  Just c -> member r c

delete :: Ord a => [a] -> Trie a -> Trie a
delete (a : r) (Trie b cs) = Trie b $ case M.lookup a cs of
  Nothing -> cs
  Just c ->
    let newChild = delete r c
    in case newChild of
      (Trie False ccs) | M.null ccs ->
        M.delete a cs
      _ -> M.insert a newChild cs
delete [] (Trie _ cs) = Trie False cs

union :: Ord a => Trie a -> Trie a -> Trie a
union (Trie aIn aChildren) (Trie bIn bChildren) =
  Trie (aIn || bIn)
    (M.fromAscListWith union (mergeLists (M.toAscList aChildren) (M.toAscList bChildren)))
 where
  mergeLists :: Ord a => [(a, Trie a)] -> [(a, Trie a)] -> [(a, Trie a)]
  mergeLists ((a, tA) : rA) ((b, tB) : rB) =
    if a <= b
      then (a, tA) : mergeLists rA ((b, tB) : rB)
      else (b, tB) : mergeLists ((a, tA) : rA) rB

  mergeLists [] x = x
  mergeLists x [] = x
