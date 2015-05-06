
module Tree where

import           Data.List
import qualified Data.Tree as T

data Tree k v
  = Empty
  | Tree k v (Tree k v) (Tree k v)
  deriving (Show)

pp :: Show k => Tree k v -> String
pp = T.drawTree . convert
 where
  convert Empty = T.Node "{}" []
  convert (Tree a l r) = T.Node (show a) [convert l, convert r]

insertT :: k -> v -> Tree k v -> Tree k v
insertT = _

lookupT :: k -> Tree k v -> Maybe v
lookupT = _

fromList :: [a] -> Tree a
fromList = foldl' (flip insertT) Empty

toList :: Tree a -> [a]
toList t = case t of
  Empty -> []
  Tree a left right -> a : toList left ++ toList right
