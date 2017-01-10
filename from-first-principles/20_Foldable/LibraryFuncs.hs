module LibraryFuncs where

import Prelude hiding (sum, product, elem, minimum, maximum, null, length, foldMap)

import qualified Data.Foldable as D
import Data.Monoid

-- 1
sum :: (Foldable t, Num a) => t a -> a
sum = getSum . D.foldMap Sum

-- 2
product :: (Foldable t, Num a) => t a -> a
product = getProduct . D.foldMap Product

-- 3
elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem x = foldr (\y b -> y == x || b) False

-- 4
minimum :: (Foldable t, Ord a) => t a -> Maybe a
minimum = foldr fn Nothing
  where
    fn x Nothing  = Just x
    fn x (Just y) = Just $ if x < y then x else y

-- 5
maximum :: (Foldable t, Ord a) => t a -> Maybe a
maximum = foldr fn Nothing
  where
    fn x Nothing  = Just x
    fn x (Just y) = Just $ if x > y then x else y

-- 6
null :: (Foldable t) => t a -> Bool
null = foldr (\_ _ -> False) True

-- 7
length :: (Foldable t) => t a -> Int
length = getSum . D.foldMap (const 1)

-- 8
toList :: (Foldable t) => t a -> [a]
toList = foldr (:) []

-- 9
fold :: (Foldable t, Monoid m) => t m -> m
fold = D.foldMap id

-- 10
-- Use foldr
foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap f = foldr (\x acc -> f x <> acc) mempty
