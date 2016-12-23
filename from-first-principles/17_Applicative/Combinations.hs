module Combinations where

import Control.Applicative (liftA3)

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combine :: [a] -> [b] -> [c] -> [(a,b,c)]
combine = liftA3 (,,)

combos :: [(Char,Char,Char)]
combos = combine stops vowels stops
