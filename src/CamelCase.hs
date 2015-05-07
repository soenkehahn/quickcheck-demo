
module CamelCase where

import           Data.Char

unCamel :: String -> String
unCamel (a : r) | isUpper a = unCamel (toLower a : r)
unCamel (a : b : r) =
  if isUpper b
    then a : '-' : unCamel (toLower b : r)
    else a : unCamel (b : r)
unCamel x = x
