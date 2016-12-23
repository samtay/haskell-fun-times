#!/usr/bin/env stack
{- stack runghc
  --resolver lts-7
  --install-ghc 
  --package QuickCheck
  --package checkers
-}
module ZipList where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- List Applicative
data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ append xs ys

instance Functor List where
  fmap _ Nil         = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure x = Cons x Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  (Cons x xs) <*> ys =
    fmap x ys `append` (xs <*> ys)

-- ZipList Applicative

newtype ZipList' a =
  ZipList' (List a)
  deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                 in ltake 3000 l
          ys' = let (ZipList' l) = ys
                 in ltake 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure = zrepeat
  (<*>) (ZipList' xs) (ZipList' ys) = ZipList' $
    lzipW ($) xs ys

zrepeat :: a -> ZipList' a
zrepeat = ZipList' . lrepeat

lrepeat :: a -> List a
lrepeat x = Cons x $ lrepeat x

lzipW :: (a -> b -> c) -> List a -> List b -> List c
lzipW _ Nil _ = Nil
lzipW _ _ Nil = Nil
lzipW f (Cons x xs) (Cons y ys) =
  Cons (f x y) (lzipW f xs ys)

ltake :: Int -> List a -> List a
ltake _ Nil         = Nil
ltake 0 _           = Nil
ltake n (Cons x xs) = Cons x $ ltake (n-1) xs


-- Testing ZipList

-- unfortunate orphan instances. Try to avoid these
-- in code you're going to keep or release.
instance Monoid a => Monoid (ZipList a) where
  -- Here, 'Ziplist []' is not an identity! It's like multiplying by zero
  mempty  = pure mempty
  mappend = liftA2 mappend

instance Arbitrary a => Arbitrary (ZipList a) where
  arbitrary = ZipList <$> arbitrary

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = ltoList <$> listOf arbitrary

ltoList :: [a] -> List a
ltoList = foldr Cons Nil

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary

instance Arbitrary a => Arbitrary (Sum a) where
  arbitrary = Sum <$> arbitrary

instance Eq a => EqProp (ZipList a) where (=-=) = eq

trigger :: [ (String, String, Int) ]
trigger = undefined

ltrigger :: List (Int, String, Int)
ltrigger = undefined

ztrigger :: ZipList' (String, String, Int)
ztrigger = undefined

main = do
  quickBatch $ monoid (ZipList [1 :: Sum Int])
  quickBatch $ applicative trigger 
  quickBatch $ applicative ztrigger 
