module AsPatterns where

import Data.Char

--1. This should return True if (and only if) all the values in the first list appear in the second list, though they need not be contiguous.
-- TODO why do I need damn as pattern? below works fine..
isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf (x:xs) ys = (x `elem` ys) && (isSubsequenceOf xs ys)

--2. Split a sentence into words, then tuple each word with the capitalized form of each.
capitalizeWords :: String -> [(String, String)]
capitalizeWords s = map f $ words s
  where f s@(c:cs) = (s, (toUpper c):cs)
