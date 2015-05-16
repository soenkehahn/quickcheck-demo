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
      property $ \ list a ->
        a `member` insert a (fromList list)

  describe "fromList" $ do
    it "complements toList" $ do
      property $ \ list ->
        sort (toList (fromList list)) === sort (nub list)

  describe "delete" $ do
    it "works" $ do
      property $ \ list ->
        not (null list) ==>
        forAll (elements list) $ \ e ->
        not (e `member` delete e (fromList list))

    it "leaves no useless branches in the trie" $ do
      property $ \ list ->
        not (null list) ==>
        forAll (elements list) $ \ e ->
        not (hasDeadBranches (delete e (fromList list)))

  describe "union" $ do
    it "does the same as naiveUnion" $ do
      property $ \ (fromList -> a) (fromList -> b) ->
        toList (a `union` b)
          `shouldBe`
        toList (a `naiveUnion` b)

hasDeadBranches :: Trie -> Bool
hasDeadBranches _root@(Trie _ cs) = any inner (M.elems cs)
  where
    inner (Trie isElem cs) =
      (null (M.toList cs) && not isElem) ||
      any hasDeadBranches (M.elems cs)

naiveUnion :: Trie -> Trie -> Trie
naiveUnion a b = foldl' (flip insert) a (toList b)
