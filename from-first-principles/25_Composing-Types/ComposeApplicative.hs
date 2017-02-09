#!/usr/bin/env stack
{- stack runghc
  --resolver lts-7
  --install-ghc
  --package QuickCheck
  --package checkers
-}

{-# LANGUAGE InstanceSigs #-}
module Compose.Applicative where

import Control.Applicative (liftA2)
import Data.Monoid (Sum)

import Test.QuickCheck (Arbitrary(..), frequency, listOf)
import Test.QuickCheck.Checkers (quickBatch, EqProp(..), eq)
import Test.QuickCheck.Classes (functor, applicative, traversable)

-- Compose Functor & Applicative

newtype Compose f g a =
  Compose { getCompose :: f (g a) }
  deriving (Eq, Show)

instance (Applicative f, Applicative g) =>
  Applicative (Compose f g) where

    pure :: a -> Compose f g a
    pure = Compose . pure . pure

    (<*>) :: Compose f g (a -> b)
          -> Compose f g a
          -> Compose f g b
    (Compose fgab) <*> (Compose fga) = Compose $ liftA2 (<*>) fgab fga

instance (Functor f, Functor g) =>
  Functor (Compose f g) where
    fmap f (Compose x) = Compose $ (fmap . fmap) f x

-- 1 Foldable

instance (Foldable f, Foldable g) =>
  Foldable (Compose f g) where
    foldMap f (Compose fga) = (foldMap . foldMap) f fga

-- 2 Traversable

instance (Traversable f, Traversable g) =>
  Traversable (Compose f g) where
    traverse f (Compose fga) =
      Compose
        <$> (traverse . traverse) f fga

-- where
-- traverse . traverse :: (Traversable t, Traversable s, Applicative f)
--                     => (a -> f b) -> s (t a) -> f (s (t b))

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- Testing stuff
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

instance Arbitrary (f (g a)) =>
  Arbitrary (Compose f g a) where
    arbitrary = Compose <$> arbitrary

instance Eq (f (g a)) => EqProp (Compose f g a) where
  (=-=) = eq

main :: IO ()
main = do
  quickBatch $ functor trigger1
  quickBatch $ applicative trigger3
  quickBatch $ traversable trigger3
    where trigger1 = undefined :: Compose Maybe [] (String, Int, String)
          trigger3 = undefined :: Compose (Either String) Maybe (String, Int, String)
