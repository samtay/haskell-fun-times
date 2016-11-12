module WordNumber where

import Data.List (intersperse)

-- | Takes digits (0 <= n < 9) and outputs word version
digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"
digitToWord _ = error "only accepts single digits"

-- | Transforms integer into an array of its digits
-- >>> digits 2345
-- [2,3,4,5]
digits :: Int -> [Int]
digits num = snd $ go num []
  where go x ds
          | x < 10    = (0, x:ds)
          | otherwise = go (x `div` 10) ((x `mod` 10):ds)

-- | Transforms integer into dash separated words
-- >>> wordNumber 2345
-- "two-three-four-five"
wordNumber :: Int -> String
wordNumber n = concat . intersperse "-" $ map digitToWord $ digits n

-- Generalized from hunD in ch 7, but didn't end up using it.
digit :: Int -> Int -> Int
digit n = snd . (`divMod`10) . fst . (`divMod`(10^n))
