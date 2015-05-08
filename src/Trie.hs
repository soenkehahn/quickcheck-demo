
module Trie where

import           Data.Map (Map)

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
