#!/usr/bin/env stack
{- stack runghc
  --resolver lts-7
  --install-ghc 
  --package QuickCheck
  --package checkers
-}
module Instances where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- 1
data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap = liftA

instance Applicative Pair where
  pure x = Pair x x
  (Pair x1 x2) <*> (Pair y1 y2) = Pair (x1 y1) (x2 y2)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Pair x y

instance Eq a => EqProp (Pair a)
  where (=-=) = eq

-- 2
data Two a b = Two a b
  deriving (Eq, Show)

instance Monoid a => Functor (Two a) where
  fmap = liftA

instance Monoid a => Applicative (Two a) where
  pure = Two mempty
  (Two x1 x2) <*> (Two y1 y2) =
    Two (x1 <> y1) (x2 y2)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Two x y

instance (Eq a,Eq b) => EqProp (Two a b)
  where (=-=) = eq

-- 3
data Three a b c = Three a b c
  deriving (Eq, Show)

instance (Monoid a, Monoid b) => Functor (Three a b) where
  fmap = liftA

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure = Three mempty mempty
  (Three x1 y1 z1) <*> (Three x2 y2 z2) =
    Three (x1 <> x2) (y1 <> y2) (z1 z2)

instance (Arbitrary a, Arbitrary b, Arbitrary c)
  => Arbitrary (Three a b c) where
    arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

-- 4
data Three' a b = Three' a b b deriving (Eq, Show)

instance (Monoid a) => Functor (Three' a) where
  fmap = liftA

instance (Monoid a) => Applicative (Three' a) where
  pure x = Three' mempty x x
  (Three' x1 y1 z1) <*> (Three' x2 y2 z2) =
    Three' (x1 <> x2) (y1 y2) (z1 z2)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = liftA3 Three' arbitrary arbitrary arbitrary

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

-- 6
data Four' a b = Four' a a a b
  deriving (Eq, Show)

instance Monoid a => Functor (Four' a) where
  fmap = liftA

instance Monoid a => Applicative (Four' a) where
  pure = Four' mempty mempty mempty
  (Four' x1 x2 x3 x4) <*> (Four' y1 y2 y3 y4) =
    Four' (x1 <> y1) (x2 <> y2) (x3 <> y3) (x4 y4)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = Four' <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a,Eq b) => EqProp (Four' a b)
  where (=-=) = eq


trigger1 :: Pair (String, String, Int)
trigger1 = undefined
trigger2 :: Two String (String, String, Int)
trigger2 = undefined
trigger3 :: Three String (Sum Int) (String, String, Int)
trigger3 = undefined
trigger4 :: Three' String (String, String, Int)
trigger4 = undefined
trigger6 :: Four' String (String, String, Int)
trigger6 = undefined

instance Arbitrary a => Arbitrary (Sum a) where
  arbitrary = Sum <$> arbitrary

main = do
  quickBatch $ applicative trigger1
  quickBatch $ applicative trigger2
  quickBatch $ applicative trigger3
  quickBatch $ applicative trigger4
  quickBatch $ applicative trigger6
