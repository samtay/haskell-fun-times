module WarmUp where

-- 1. Given the following sets of consonants and vowels:
stops = "pbtdkg"
vowels = "aeiou"
-- a) Write a function that takes inputs from stops and vowels and makes 3-tuples of all possible stop-vowel-stop combinations. These will not all correspond to real words in English, although the stop-vowel-stop pattern is common enough that many of them will.
tupz :: [String]
tupz = [s1:v:s2:[] | s1 <- stops, v <- vowels, s2 <- stops]

-- b) Modify that function so that it only returns the combinations that begin with a p.
tupzP :: [String]
tupzP = [ tup | tup <- tupz, head tup == 'p']

-- c) Now set up lists of nouns and verbs (instead of stops and vowels) and modify the function to make tuples representing possible noun-verb-noun sentences.
generalAF :: [String] -> [String] -> [String]
generalAF nouns verbs =
  [ noun1 ++ " " ++ verb ++ " " ++ noun2 | noun1 <- nouns, verb <- verbs, noun2 <- nouns ]

