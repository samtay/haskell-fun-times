-- 5 Kyu
module Kata (possibilities) where

possibilities :: String -> [String]
possibilities = traverse $ \c -> if c == '?' then "01" else [c]
