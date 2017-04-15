-- 5 Kyu
module Codewars.G964.Primeconsec where

import Data.List (tails)

consecKprimes :: Int -> [Int] -> Int
consecKprimes k = cntPairTrues . map (isKprime k)

cntPairTrues :: [Bool] -> Int
cntPairTrues = length . filter go . tails
  where go (True:True:_) = True
        go _             = False

isKprime :: Int -> Int -> Bool
isKprime k = (==k) . length . factor

factor :: Integral t => t -> [t]
factor 1 = []
factor n = let divisors = dropWhile ((/= 0) . mod n) [2 .. ceiling $ sqrt $ fromIntegral n]
             in let prime = if null divisors then n else head divisors
                  in (prime :) $ factor $ div n prime
