{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
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
data Stream a = Cons a (Stream a)

maxVisible :: Integer
maxVisible = 20
instance Show a => Show (Stream a) where
  show = ("Stream(" ++) . (++ "...)") . go maxVisible
    where go n (Cons x xs)
            | n == 0 = ""
            | otherwise = show x ++ ", " ++ go (n-1) xs

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

-- Exercise 4
streamRepeat :: a -> Stream a
streamRepeat x = Cons x $ streamRepeat x

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap = fmap

instance Functor Stream where
  fmap f (Cons x xs) = Cons (f x) $ fmap f xs

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons (f x) $ streamFromSeed f (f x)

-- Exercise 5
nats :: Stream Integer
nats = streamFromSeed (+1) 0

ruler :: Stream Integer
ruler = go 0
  where go n = interleaveStreams (streamRepeat n) (go $ n + 1)

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x xs) ys = Cons x (interleaveStreams ys xs)

-- Exercise 6 (extra credit)
-- My wheelhouse! Using streams to represent infinite polynomials in generating functions
x1 :: Stream Integer
x1 = Cons 0 $ Cons 1 $ streamRepeat 0

instance Num (Stream Integer) where
  fromInteger n = Cons n $ streamRepeat 0
  negate = fmap negate
  (Cons x xs) + (Cons y ys) = Cons (x + y) $ xs + ys
  (Cons x xs) * (Cons y ys) =
    Cons (x * y) $ ( (fromInteger x) * ys
                   + xs * (Cons y ys)
                   )

instance Fractional (Stream Integer) where
  xss@(Cons x xs) / yss@(Cons y ys) =
    Cons (x `div` y) $ (Cons (1 `div` y) $ streamRepeat 0) * (xs - (xss / yss) * ys)

fibs3 :: Stream Integer
fibs3 = x1 / (1 - x1 - x1^(2::Integer))

-- Exercise 7 (extra credit)
data M = M Integer Integer
           Integer Integer
  deriving (Eq, Show)

instance Num M where
  (M a b c d) * (M w x y z) = M (a*w + b*y) (a*x + b*z) (c*w + d*y) (c*x + d*z)

fib4 :: Integer -> Integer
fib4 n = let (M _ m _ _) = (M 1 1 1 0)^n in m
