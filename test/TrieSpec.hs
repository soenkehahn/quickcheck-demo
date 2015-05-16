{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module TrieSpec where

import           Data.List (foldl')
import           Data.List (nub, sort)
import qualified Data.Map as M
import           Test.Hspec
import           Test.QuickCheck

import           Trie

spec :: Spec
spec = do
  describe "insert" $ do
    it "works" $ do
      property $ \ list (a :: String) ->
        a `member` insert a (fromList list)

  describe "fromList" $ do
    it "complements toList" $ do
      property $ \ (list :: [String]) ->
        sort (toList (fromList list)) === sort (nub list)

  describe "delete" $ do
    it "works" $ do
      property $ \ (list :: [String]) ->
        not (null list) ==>
        forAll (elements list) $ \ e ->
        not (e `member` delete e (fromList list))

    it "leaves no useless branches in the trie" $ do
      property $ \ (list :: [String]) ->
        not (null list) ==>
        forAll (elements list) $ \ e ->
        not (hasDeadBranches (delete e (fromList list)))

  describe "union" $ do
    it "does the same as naiveUnion" $ do
      property $ \ (fromList -> (a :: Trie Char)) (fromList -> b) ->
        toList (a `union` b)
          `shouldBe`
        toList (a `naiveUnion` b)

hasDeadBranches :: Trie a -> Bool
hasDeadBranches _root@(Trie _ cs) = any inner (M.elems cs)
  where
    inner (Trie isElem cs) =
      (null (M.toList cs) && not isElem) ||
      any hasDeadBranches (M.elems cs)

naiveUnion :: Ord a => Trie a -> Trie a -> Trie a
naiveUnion a b = foldl' (flip insert) a (toList b)
