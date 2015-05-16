{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module TreeSpec where

import           Control.Arrow
import           Data.Function
import           Data.List
import           Test.Hspec
import           Test.QuickCheck

import           Tree

spec :: Spec
spec = do
  describe "insert" $ do
    it "works" $ do
      property $ \ tree (k :: Int) (v :: String) ->
        counterexample (pp (insertT k v tree)) $
        lookupT k (insertT k v tree)
          `shouldBe` Just v

  describe "delete" $ do
    it "works" $
      property $ \ (tree :: Tree Int String) ->
        forAll (elements (keys tree)) $ \ key ->
          lookupT key (deleteT key tree) === Nothing

  describe "fromList" $ do
    it "complements toList" $ do
      property $ \ (list :: [(Int, String)]) ->
        counterexample (pp (fromList list)) $
        toList (fromList list) `shouldBe`
          sortBy (compare `on` fst) (nubBy ((==) `on` fst) (reverse list))

    it "complements toList" $ do
      property $ \ (tree :: Tree Int String) ->
        counterexample ("input:\n" ++ pp tree) $
        counterexample ("result:\n" ++ pp (fromList (toList tree))) $
        fromList (toList tree) `shouldBe` tree

    it "returns sorted trees" $ do
      property $ \ (tree :: Tree String Int) ->
        isSorted tree

    it "returns balanced trees" $ do
      property $ \ (tree :: Tree String Int) ->
        counterexample (pp tree) $
        isBalanced tree

instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (Tree k v) where
  arbitrary = fromList <$> arbitrary
  shrink = toList >>> shrink >>> map fromList

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
    ((height l - height r) `elem` [0, -1])
  Empty -> True

height :: Tree k v -> Integer
height t = case t of
  Tree _ _ l r -> succ (max (height l) (height r))
  Empty -> 0
