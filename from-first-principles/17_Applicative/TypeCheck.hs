{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module TypeCheck where

import Control.Applicative
import Data.List

-- 1
added :: Maybe Integer
added = (+3) <$> lookup 3 (zip [1, 2, 3] [4, 5, 6])

y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

-- 2
tupled :: Maybe (Integer, Integer)
tupled = liftA2 (,) y z

-- 3
x :: Maybe Int
x = elemIndex 3 [1, 2, 3, 4, 5]

w :: Maybe Int
w = elemIndex 4 [1, 2, 3, 4, 5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> x <*> w

-- 4
xs :: [Integer]
xs = [1, 2, 3]
ys :: [Integer]
ys = [4, 5, 6]

xx :: Maybe Integer
xx = lookup 3 $ zip xs ys

yy :: Maybe Integer
yy = lookup 2 $ zip xs ys

-- note this just "sums" the first integer of the tuple,
-- seems like thats what author wants?
summed :: Maybe Integer
summed = sum <$> ((,) <$> x <*> y)
