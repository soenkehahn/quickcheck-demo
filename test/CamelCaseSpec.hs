
module CamelCaseSpec where

import           Data.Char
import           Test.Hspec
import           Test.QuickCheck

import           CamelCase

spec :: Spec
spec = do
  describe "unCamel" $ do
    it "replaces upper case characters with dashes" $ do
      unCamel "fooBar" `shouldBe` "foo-bar"

    it "always replaces upper case characters with dashes" $ do
      property $ \ prefix suffix ->
        forAll (elements lowers) $ \ lower ->
        forAll (elements uppers) $ \ upper ->
        let input = prefix ++ [lower, upper] ++ suffix
            result = unCamel input
        in counterexample (show input ++ " -> " ++ show result) $
        result `shouldBe`
          (unCamel prefix ++ unCamel ([lower, '-', toLower upper] ++ suffix))

    it "returns no uppercase letters" $ do
      property $ \ string ->
        counterexample (unCamel string) $
        unCamel string `shouldSatisfy` all (not . isUpper)

    it "should only return dashes and lowercase letters" $ do
      property $ forAll (listOf (elements (lowers ++ uppers))) $ \ string ->
        unCamel string `shouldSatisfy` all (`elem` ('-' : ['a' .. 'z']))

    it "is idempotent" $ do
      property $ \ string ->
        unCamel (unCamel string) `shouldBe` unCamel string

lowers :: [Char]
lowers = ['a' .. 'z']

uppers :: [Char]
uppers = ['A' .. 'Z']
