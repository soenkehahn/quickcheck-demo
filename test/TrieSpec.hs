{-# LANGUAGE ViewPatterns #-}

module TrieSpec where

import           Data.List (nub, sort)
import qualified Data.Map as M
import           Test.Hspec
import           Test.QuickCheck

import           Trie

spec :: Spec
spec = do
  describe "insert" $ do
    it "works" $ do
      pending

  describe "fromList" $ do
    it "complements toList" $ do
      pending

  describe "delete" $ do
    it "works" $ do
      pending

    it "leaves no useless branches in the trie" $ do
      pending

hasDeadBranches :: Trie -> Bool
hasDeadBranches _root@(Trie _ cs) = any inner (M.elems cs)
  where
    inner (Trie isElem cs) =
      (null (M.toList cs) && not isElem) ||
      any hasDeadBranches (M.elems cs)
