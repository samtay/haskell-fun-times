module Cipher where

import Data.List

-- Instructions say to leave non alphabetical chars where they are, hence custom ord/chr funcs

-- | Encode a string via Ceaser cipher by shifting characters according to args
-- >>> ceaser 2 "abc"
-- "cde"
ceaser :: Int -> String -> String
ceaser n = map (\c -> shift n c)

-- | Decode a string via Ceaser cipher by unshifting characters according to args
-- >>> unCeaser 2 "cde"
-- "abc"
unCeaser :: Int -> String -> String
unCeaser n = map (\c -> shift (-n) c)

-- | Shifts alpha characters
shift :: Int -> Char -> Char
shift n c = case ord c of
                 Nothing -> c
                 Just x -> chr (x + n)

-- | Get index of 52 character list
ord :: Char -> Maybe Int
ord c = findIndex (==c) $ alphabet

-- | Get character by index
chr :: Int -> Char
chr = (alphabet!!) . (`mod`52)

-- | Shortcut to alphabet list
alphabet :: [Char]
alphabet = ['a'..'z'] ++ ['A'..'Z']
