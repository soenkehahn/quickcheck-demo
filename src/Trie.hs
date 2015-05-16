{-# LANGUAGE DeriveGeneric #-}

module Trie where

import           Control.DeepSeq
import           Data.List (foldl')
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           GHC.Generics

data Trie
  = Trie Bool (Map Char Trie)
  deriving (Show, Generic)

instance NFData Trie

pp :: Trie -> String
pp = error "pp"

empty :: Trie
empty = Trie False M.empty

insert :: String -> Trie -> Trie
insert "" (Trie _ cs) = Trie True cs
insert (a : r) (Trie b cs) = case M.lookup a cs of
  Nothing -> Trie b (M.insert a (insert r empty) cs)
  Just c -> Trie b (M.insert a (insert r c) cs)

fromList :: [String] -> Trie
fromList = foldl' (flip insert) empty

toList :: Trie -> [String]
toList (Trie b cs) =
  (if b then [""] else []) ++
  concat (map (\ (c, t) -> map (c :) (toList t)) (M.toList cs))

member :: String -> Trie -> Bool
member [] (Trie b _) = b
member (a : r) (Trie _ cs) = case M.lookup a cs of
  Nothing -> False
  Just c -> member r c

delete :: String -> Trie -> Trie
delete "" (Trie _ cs) = Trie False cs
delete (a : r) (Trie b cs) = Trie b $ case M.lookup a cs of
  Nothing -> cs
  Just c ->
    let newChild = delete r c
    in case newChild of
      (Trie False ccs) | M.null ccs ->
        M.delete a cs
      _ -> M.insert a newChild cs

union :: Trie -> Trie -> Trie
union (Trie aIn aChildren) (Trie bIn bChildren) =
  Trie (aIn || bIn)
    (M.fromAscListWith union (mergeLists (M.toAscList aChildren) (M.toAscList bChildren)))
 where
  mergeLists :: [(Char, Trie)] -> [(Char, Trie)] -> [(Char, Trie)]
  mergeLists ((a, tA) : rA) ((b, tB) : rB) =
    if a <= b
      then (a, tA) : mergeLists rA ((b, tB) : rB)
      else (b, tB) : mergeLists ((a, tA) : rA) rB

  mergeLists [] x = x
  mergeLists x [] = x
