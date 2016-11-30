module StringProcessing where

-- 1. Write a recursive function named replaceThe which takes a text/string, breaks it into words and replaces each instance of “the” with “a”.  It’s intended only to replace exactly the word “the”. notThe is a suggested helper function for accomplishing this.

-- example GHCi session above the functions
-- >>> notThe "the"
-- Nothing
-- >>> notThe "blahtheblah"
-- Just "blahtheblah"
-- >>> notThe "woot"
-- Just "woot"
notThe :: String -> Maybe String
notThe "the" = Nothing
notThe s     = Just s

-- >>> replaceThe "the cow loves us"
-- "a cow loves us"
replaceThe :: String -> String
replaceThe = unwords . map go . words
  where go = maybe "a" id . notThe

-- 2. Write a recursive function that takes a text/string, breaks it into words, and counts the number of instances of ”the” followed by a vowel-initial word.

-- >>> countTheBeforeVowel "the cow"
-- 0
-- >>> countTheBeforeVowel "the evil cow"
-- 1
countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = fst . (foldl go (0, False)) . words
  where go (c, lastThe) w =
          let nowVowel = head w `elem` "aeiou"
              nowThe   = w == "the"
           in if (lastThe && nowVowel) then (c+1, nowThe) else (c, nowThe)

-- 3. Return the number of letters that are vowels in a word.  Hint: it’s helpful to break this into steps. Add any helper functions necessary to achieve your objectives.

--      a) Test for vowelhood
--      b) Return the vowels of a string
--      c) Count the number of elements returned

-- >>> countVowels "the cow"
-- 2
-- >>> countVowels "Mikolajczak"
-- 4
countVowels :: String -> Integer
countVowels = toInteger . length . filter (`elem` "aeiou")
