#!/usr/bin/env stack
-- stack runghc --resolver lts-7 --install-ghc --package QuickCheck
{-# LANGUAGE ViewPatterns #-}
module FunctorQuickCheck where

import Test.QuickCheck
import Test.QuickCheck.Function

functoryIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functoryIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) =>
     (a -> b)
  -> (b -> c)
  -> f a
  -> Bool
functorCompose f g x =
  (fmap g (fmap f x)) == (fmap (g . f) x)

functorCompose' :: (Eq (f c), Functor f) =>
     f a
  -> Fun a b
  -> Fun b c
  -> Bool
functorCompose' x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x)

type IntToInt = Fun Int Int
type IntFC = [Int] -> IntToInt -> IntToInt -> Bool

-- 16.10 Instances
type F f a = f a -> Fun a a -> Fun a a -> Bool
-----------------------1-------------------------
newtype Identity a = Identity a deriving (Eq, Show)
instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance (Arbitrary a) => Arbitrary (Identity a) where
    arbitrary = arbitrary >>= return . Identity

type IdentityF = F Identity Int

-----------------------2-------------------------
data Pair a = Pair a a deriving (Eq, Show)
instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Pair x y

type PairF = F Pair Int

-----------------------6-------------------------
data Four a b c d = Four a b c d deriving (Eq, Show)
instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Arbitrary a,Arbitrary b,Arbitrary c,Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = Four <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

type FourF = F (Four Int Int Int) Int

-----------------------8-------------------------
-- Can `data Trivial = Trivial` have a Functor instance?

-- NO! Functors must have kind * -> *.

------------------- execution -------------------
main :: IO ()
main = do
  quickCheck $ \x -> functoryIdentity (x :: [Int])
  quickCheck $ \x -> functorCompose (+1) (*2) (x :: [Int]) -- only checks law on two functions
  quickCheck (functorCompose' :: IntFC)

  quickCheck $ \x -> functoryIdentity (x :: Identity Int)
  quickCheck (functorCompose' :: IdentityF)

  quickCheck $ \x -> functoryIdentity (x :: Pair Int)
  quickCheck (functorCompose' :: PairF)

  quickCheck $ \x -> functoryIdentity (x :: Four Int Int Int Int)
  quickCheck (functorCompose' :: FourF)
