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

------------------- execution -------------------
main :: IO ()
main = do
  quickCheck $ \x -> functoryIdentity (x :: [Int])
  quickCheck $ \x -> functorCompose (+1) (*2) (x :: [Int]) -- only checks law on two functions
  quickCheck (functorCompose' :: IntFC)
