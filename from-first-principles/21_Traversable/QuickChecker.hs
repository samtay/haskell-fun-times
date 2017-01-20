#!/usr/bin/env stack
{- stack runghc
  --resolver lts-7
  --install-ghc
  --package QuickCheck
  --package checkers
-}
module QuickChecking where

import Test.QuickCheck (Arbitrary(..))
import Test.QuickCheck.Checkers (quickBatch, EqProp(..), eq)
import Test.QuickCheck.Classes (traversable)

-- 1 Identity
newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

instance Foldable Identity where
  foldMap f (Identity x) = f x

instance Traversable Identity where
  traverse f (Identity x) = Identity <$> f x

----------------- Testing stuff -----------------

-- Arbitrary instances
instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance (Eq a) => EqProp (Identity a) where
  (=-=) = eq

-- Trigger type
type Trigger = (String, Int, String)
trigger = undefined

-- IO runtime
main =
  quickBatch $ traversable (trigger :: Identity Trigger)
