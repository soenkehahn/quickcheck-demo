{-# LANGUAGE ScopedTypeVariables #-}

module TreeSpec where

import           Data.Function
import           Data.List
import           Test.Hspec
import           Test.QuickCheck

import           Tree

spec :: Spec
spec = do
  describe "insert" $ do
    it "works" $ do
      -- @shahn: use Arbitrary (Tree k v)
      pending

  describe "fromList" $ do
    it "complements toList" $ do
      pending

    it "returns sorted trees" $ do
      pending

    it "returns balanced trees" $ do
      pending

isSorted :: Ord k => Tree k v -> Bool
isSorted Empty = True
isSorted (Tree k _ left right) =
  all ((< k) . fst) (toList left) &&
  isSorted left &&
  all ((> k) . fst) (toList right) &&
  isSorted right

isBalanced :: Tree k v -> Bool
isBalanced t = case t of
  Tree _ _ l r ->
    isBalanced l &&
    isBalanced r &&
    (abs (height l - height r) <= 1)
  Empty -> True

height :: Tree k v -> Integer
height t = case t of
  Tree _ _ l r ->
    succ (max (height l) (height r))
  Empty -> 0
