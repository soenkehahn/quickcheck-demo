
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
insertT = _

lookupT :: Ord k => k -> Tree k v -> Maybe v
lookupT = _

fromList :: Ord k => [(k, v)] -> Tree k v
fromList = _

toList :: Tree k v -> [(k, v)]
toList = _
