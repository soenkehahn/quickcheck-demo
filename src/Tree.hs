
module Tree where

import           Data.List
import qualified Data.Tree as T

data Tree k v
  = Empty
  | Tree k v (Tree k v) (Tree k v)
  deriving (Show, Eq)

pp :: Show k => Tree k v -> String
pp = T.drawTree . convert
 where
  convert Empty = T.Node "{}" []
  convert (Tree k _ l r) = T.Node (show k) [convert l, convert r]

keys :: Tree k v -> [k]
keys t = case t of
  Empty -> []
  Tree k _ l r -> keys l ++ [k] ++ keys r

insertT :: Ord k => k -> v -> Tree k v -> Tree k v
insertT k v t = case t of
  Empty -> Tree k v Empty Empty
  Tree k' v' l r -> case compare k k' of
    EQ -> Tree k v l r
    LT -> Tree k' v' (insertT k v l) r
    GT -> Tree k' v' l (insertT k v r)

lookupT :: Ord k => k -> Tree k v -> Maybe v
lookupT needle t = case t of
  Tree k v l r -> case compare needle k of
    EQ -> Just v
    LT -> lookupT needle l
    GT -> lookupT needle r
  Empty -> Nothing

deleteT :: k -> Tree k v -> Tree k v
deleteT _needle _t = _

fromList :: Ord k => [(k, v)] -> Tree k v
fromList = foldl' (flip (uncurry insertT)) Empty

toList :: Tree k v -> [(k, v)]
toList t = case t of
  Empty -> []
  Tree k v l r -> toList l ++ [(k, v)] ++ toList r
