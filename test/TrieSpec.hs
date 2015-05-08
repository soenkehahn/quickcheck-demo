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
      property $ \ list a ->
        insert a (fromList list) `shouldSatisfy` (a `member`)

  describe "fromList" $ do
    it "complements toList" $ do
      property $ \ list ->
        counterexample (pp (fromList list)) $
        toList (fromList list) `shouldBe` sort (nub list)

  describe "delete" $ do
    it "works" $ do
      property $ \ list ->
        not (null list) ==>
        forAll (elements list) $ \ e ->
          counterexample (pp (fromList list)) $
          delete e (fromList list) `shouldSatisfy` (not . (e `member`))

    it "leaves no useless branches in the trie" $ do
      property $ \ list ->
        not (null list) ==>
        forAll (elements list) $ \ e ->
          counterexample (pp (fromList list)) $
          not (hasDeadBranches (delete e (fromList list)))

hasDeadBranches :: Trie -> Bool
hasDeadBranches _root@(Trie _ cs) = any inner (M.elems cs)
  where
    inner (Trie isElem cs) =
      (null (M.toList cs) && not isElem) ||
      any hasDeadBranches (M.elems cs)
