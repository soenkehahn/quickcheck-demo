
module CamelCase where

import           Data.Char

unCamel :: String -> String
unCamel (x : xs)
  | isUpper x = '-' : toLower x : unCamel xs
  | otherwise = x : unCamel xs
unCamel [] = []
