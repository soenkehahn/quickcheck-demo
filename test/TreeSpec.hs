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
      property $ \ l (k :: Integer) (v :: String) ->
        counterexample (pp (fromList l)) $
        lookupT k (insertT k v (fromList l)) `shouldBe` Just v

  describe "fromList" $ do
    it "complements toList" $ do
      property $ \ (list :: [(Integer, String)]) ->
        toList (fromList list) `shouldBe`
          sort (nubBy ((==) `on` fst) (reverse list))

    it "returns sorted trees" $ do
      property $ \ (list :: [(Integer, String)]) ->
        counterexample (pp (fromList list)) $
        isSorted (fromList list)

isSorted :: Ord k => Tree k v -> Bool
isSorted Empty = True
isSorted (Tree k _ left right) =
  all ((< k) . fst) (toList left) &&
  isSorted left &&
  all ((> k) . fst) (toList right) &&
  isSorted right
