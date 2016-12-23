#!/usr/bin/env stack
{- stack runghc
  --resolver lts-7
  --install-ghc 
  --package QuickCheck
  --package checkers
-}
module Validation where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck hiding (Success, Failure)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- Validation Applicative

data Validation err a =
    Failure err
  | Success a
  deriving (Eq, Show)

{-
-- Using liftA requires Monoid constraint

instance Monoid e => Functor (Validation e) where
  fmap = liftA
-}

instance Functor (Validation e) where
  fmap _ (Failure x) = Failure x
  fmap f (Success y) = Success (f y)

instance Monoid e => Applicative (Validation e) where
  pure = Success
  (Failure x) <*> (Failure y) = Failure $ x <> y
  (Failure x) <*> _           = Failure x
  _ <*> (Failure x)           = Failure x
  (Success x) <*> (Success y) = Success $ x y

-- Testing

instance (Arbitrary a, Arbitrary e) => Arbitrary (Validation e a) where
  arbitrary = do
    e <- arbitrary
    a <- arbitrary
    elements [Failure e, Success a]

instance (Eq a, Eq e) => EqProp (Validation e a) where (=-=) = eq

trigger :: Validation String (Int, Int, String)
trigger = undefined

main = quickBatch $ applicative trigger 
