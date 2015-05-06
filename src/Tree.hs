
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
  convert (Tree k _ l r) = T.Node (show k) [convert l, convert r]

insertT :: Ord k => k -> v -> Tree k v -> Tree k v
insertT ki vi t = case t of
  Empty -> Tree ki vi Empty Empty
  Tree k v l r -> case compare ki k of
    EQ -> Tree ki vi l r
    LT -> Tree k v (insertT ki vi l) r
    GT -> Tree k v l (insertT ki vi r)

lookupT :: Ord k => k -> Tree k v -> Maybe v
lookupT _ Empty = Nothing
lookupT needle (Tree k v l r) = case compare needle k of
  EQ -> Just v
  LT -> lookupT needle l
  GT -> lookupT needle r

fromList :: Ord k => [(k, v)] -> Tree k v
fromList = foldl' (flip (uncurry insertT)) Empty

toList :: Tree k v -> [(k, v)]
toList t = case t of
  Empty -> []
  Tree k v left right ->
    toList left ++
    (k, v) :
    toList right
