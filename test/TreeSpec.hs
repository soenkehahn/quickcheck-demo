{-# LANGUAGE ScopedTypeVariables #-}

module TreeSpec where

import           Test.Hspec
import           Test.QuickCheck

import           Tree

spec :: Spec
spec = do
  describe "insert" $ do
    it "works" $ do
      property $ \ l a ->
        lookupT a (insertT a (fromList l)) `shouldBe` Just a

  describe "fromList" $ do
    it "complements toList" $ do
      property $ \ (list :: [Integer]) ->
        toList (fromList list) `shouldBe` list

    it "returns sorted trees" $ do
      property $ \ (list :: [Integer]) ->
        counterexample (pp (fromList list)) $
        isSorted (fromList list)

isSorted :: Ord a => Tree a -> Bool
isSorted Empty = True
isSorted (Tree a left right) =
  all (< a) (toList left) &&
  isSorted left &&
  all (> a) (toList right) &&
  isSorted right
