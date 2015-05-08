
module CamelCase where

import           Data.Char

unCamel :: String -> String
unCamel (a : b : r)
  | isUpper b = toLower a : '-' : unCamel (toLower b : r)
  | otherwise = toLower a : unCamel (b : r)
unCamel [x] = [toLower x]
unCamel "" = ""
