#!/usr/bin/env stack
{- stack runghc
  --resolver lts-7
  --install-ghc
  --package QuickCheck
  --package checkers
-}
module Instances where

import Prelude hiding (Left, Right)
import Control.Monad (ap, liftM)
import Data.Monoid ((<>))
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- Monad instances
-- 1
data Nope a = NopeDotJpg deriving (Eq, Show)

instance Monad Nope where
  return _ = NopeDotJpg
  _ >>= _ = NopeDotJpg

-- 2
data PhhhbbtttEither b a =
    Left a
  | Right b
  deriving (Eq, Show)

instance Monad (PhhhbbtttEither b) where
  return          = Left
  (Left a) >>= f  = f a
  (Right b) >>= f = Right b

-- 3
newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Monad Identity where
  return = Identity
  (Identity x) >>= f = f x

-- 4
data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Monoid (List a) where
  mempty                 = Nil
  mappend Nil ys         = ys
  mappend (Cons x xs) ys = Cons x $ mappend xs ys

instance Monad List where
  return x          = Cons x Nil
  Nil >>= _         = Nil
  (Cons x xs) >>= f = f x <> (xs >>= f)

-- Superclasses
instance Functor Nope where
  fmap _ _ = NopeDotJpg

instance Applicative Nope where
  pure = return
  _ <*> _ = NopeDotJpg

instance Functor (PhhhbbtttEither b) where
  fmap = liftM

instance Applicative (PhhhbbtttEither b) where
  pure  = return
  (<*>) = ap

instance Functor Identity where
  fmap = liftM

instance Applicative Identity where
  pure  = return
  (<*>) = ap

instance Functor List where
  fmap = liftM

instance Applicative List where
  pure  = return
  (<*>) = ap

-- Testing

instance EqProp (Nope a)
  where (=-=) = eq

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

trigger1 :: Nope (String, String, Int)
trigger1 = undefined

instance (Eq a, Eq b) =>
  EqProp (PhhhbbtttEither b a)
    where (=-=) = eq

instance (Arbitrary a, Arbitrary b) =>
  Arbitrary (PhhhbbtttEither b a) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      elements [Left a, Right b]

trigger2 :: PhhhbbtttEither String (String, String, Int)
trigger2 = undefined

instance (Eq a) => EqProp (Identity a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

trigger3 :: Identity (Int, Int, Int)
trigger3 = undefined

instance (Eq a) => EqProp (List a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = ltoList <$> listOf arbitrary

ltoList :: [a] -> List a
ltoList = foldr Cons Nil

trigger4 :: List (Int, String, Int)
trigger4 = undefined

main = do
  quickBatch $ monad trigger1
  quickBatch $ monad trigger2
  quickBatch $ monad trigger3
  quickBatch $ monad trigger4
