module LanguageExercises where

import Data.Char

-- 1. Write a function that capitalizes a word.
capitalizeWord :: String -> String
capitalizeWord "" = ""
capitalizeWord (c:cs) = toUpper c : cs

-- 2. Write a function that capitalizes sentences in a paragraph. Recognize when a new sentence has begun by checking for periods.  Reuse the capitalizeWord function.
capitalizeParagraph :: String -> String
capitalizeParagraph s = (unwords . cp . words) (capitalizeWord s)
  where cp (x:y:xs) = if last x == '.' then x : capitalizeWord y : cp xs else x : cp (y:xs)
        cp xs = xs
