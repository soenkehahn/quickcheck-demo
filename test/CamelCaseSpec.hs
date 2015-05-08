
module CamelCaseSpec where

import           Control.DeepSeq
import           Data.Char
import           Data.List
import           Test.Hspec
import           Test.QuickCheck

import           CamelCase

spec :: Spec
spec = do
  describe "unCamel" $ do
    it "replaces upper case characters with dashes" $ do
      unCamel "fooBar" `shouldBe` "foo-bar"

    it "returns no uppercase letters" $ do
      property $ \ string ->
        unCamel string `shouldSatisfy` all (not . isUpper)

    it "is idempotent" $ do
      property $ \ string ->
        unCamel (unCamel string) `shouldBe` unCamel string

    it "returns strings that start with a letter" $ do
      property $ \ string ->
        counterexample (show (unCamel string)) $
        (not . ("-" `isPrefixOf`)) (unCamel string)

    it "doesn't crash" $ do
      property $ \ string ->
        deepseq (unCamel string) True

lowers :: [Char]
lowers = ['a' .. 'z']

uppers :: [Char]
uppers = ['A' .. 'Z']
