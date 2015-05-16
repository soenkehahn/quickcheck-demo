
module Trie where

import           Data.List (foldl')
import           Data.Map (Map)
import qualified Data.Map as M

data Trie
  = Trie Bool (Map Char Trie)
  deriving Show

pp :: Trie -> String
pp = _

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
union = _
