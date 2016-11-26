module LanguageExercises where

import Data.Char

-- 1. Write a function that capitalizes a word.
capitalizeWord :: String -> String
capitalizeWord "" = ""
capitalizeWord (c:cs) = (toUpper c):cs

-- 2. Write a function that capitalizes sentences in a paragraph. Recognize when a new sentence has begun by checking for periods.  Reuse the capitalizeWord function.
capitalizeParagraph :: String -> String
capitalizeParagraph s = (unwords . go . words) (capitalizeWord s)
  where go (x:y:xs) = if last x == '.' then x:(capitalizeWord y):(go xs) else x:y:(go xs)
        go xs = xs
