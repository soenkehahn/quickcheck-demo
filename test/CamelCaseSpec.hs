
module CamelCaseSpec where

import           Data.Char
import           Test.Hspec
import           Test.QuickCheck

import           CamelCase

spec :: Spec
spec = do
  describe "unCamel" $ do
    it "replaces upper case characters with dashes" $ do
      pending

    it "returns no uppercase letters" $ do
      pending

    it "is idempotent" $ do
      pending

    it "always replaces upper case characters with dashes" $ do
      pending

lowers :: [Char]
lowers = ['a' .. 'z']

uppers :: [Char]
uppers = ['A' .. 'Z']
