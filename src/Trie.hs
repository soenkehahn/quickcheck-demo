
module Trie where

import           Data.List (foldl')
import           Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Tree as T

data Trie
  = Trie Bool (Map Char Trie)
  deriving Show

pp :: Trie -> String
pp = _

empty :: Trie
empty = _

insert :: String -> Trie -> Trie
insert = _

fromList :: [String] -> Trie
fromList = _

toList :: Trie -> [String]
toList = _

member :: String -> Trie -> Bool
member = _

delete :: String -> Trie -> Trie
delete = _
