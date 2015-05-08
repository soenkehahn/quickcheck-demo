
module Trie where

import           Data.List (foldl')
import           Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Tree as T

data Trie
  = Trie Bool (Map Char Trie)
  deriving Show

pp :: Trie -> String
pp = T.drawTree . convert Nothing
  where
    convert :: Maybe Char -> Trie -> T.Tree String
    convert mChar (Trie b cs) =
      let label = (maybe "ROOT" ((++ " ") . show) mChar) ++ " " ++ if b then "IN" else "OUT"
      in T.Node label (map (\ (char, child) -> convert (Just char) child) (M.toList cs))

empty :: Trie
empty = Trie False M.empty

insert :: String -> Trie -> Trie
insert [] (Trie _ cs) = Trie True cs
insert (a : r) (Trie isElem cs) = Trie isElem $ case M.lookup a cs of
  Nothing -> M.insert a (insert r empty) cs
  Just c -> M.insert a (insert r c) cs

fromList :: [String] -> Trie
fromList = foldl' (flip insert) empty

toList :: Trie -> [String]
toList (Trie isElem cs) =
  (if isElem then [""] else []) ++
  concat (map (\ (c, trie) -> map (c :) (toList trie)) (M.toList cs))

member :: String -> Trie -> Bool
member = _

delete :: String -> Trie -> Trie
delete = _
