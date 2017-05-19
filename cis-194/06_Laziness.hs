module Laziness where

-- Exercise 1
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = fib <$> [0..]

-- Exercise 2
fibs2 :: [Integer]
fibs2 = go 0 1
  where go x y = x : go y (x + y)

-- Exercise 3
data Stream a = Stream a
  deriving (Show)

streamToList :: Stream a -> [a]
streamToList = undefined
