module Intro where

-- Exercise 1

toDigits :: Integer -> [Integer]
toDigits num = snd $ go num []
  where go x ds
          | x <= 0    = (0, [])
          | x < 10    = (0, x:ds)
          | otherwise = go (x `div` 10) ((x `mod` 10):ds)


toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

-- Exercise 2

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther ds = reverse $ map go $ zip [0..] $ reverse ds
  where go (i,d) = if i `mod` 2 == 0 then d else d * 2

-- Exercise 3

sumDigits :: [Integer] -> Integer
sumDigits = sum . concatMap toDigits

-- Exercise 4

validate :: Integer -> Bool
validate card = sumcheck `mod` 10 == 0
  where sumcheck = sumDigits . doubleEveryOther . toDigits $ card

-- Exercise 5

type Peg = String
type Move = (Peg, Peg)

-- | Given the number of discs and names for the three pegs, hanoi
-- should return a list of moves to be performed to move the stack of
-- discs from the first peg to the second
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n source dest c = hanoi (n-1) source c dest
             ++ [(source,dest)]
             ++ hanoi (n-1) c dest source

-- Exercise 6
hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 n source dest c d
  | n < 3     = hanoi n source dest c -- this problem is the same for n < 3 disks
  | otherwise = hanoi4 (n-2) source d dest c
             ++ hanoi 2 source dest c
             ++ hanoi4 (n-2) d dest source c
