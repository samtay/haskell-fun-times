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
import Test.QuickCheck.Classes (functor, applicative)

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

-- Testing stuff

instance Arbitrary (f (g a)) =>
  Arbitrary (Compose f g a) where
    arbitrary = Compose <$> arbitrary

instance Eq (f (g a)) => EqProp (Compose f g a) where
  (=-=) = eq

main :: IO ()
main = do
  quickBatch $ functor trigger1
  quickBatch $ functor trigger3
  quickBatch $ applicative trigger1
  quickBatch $ applicative trigger3
    where trigger1 = undefined :: Compose Maybe [] (String, Int, String)
          trigger3 = undefined :: Compose (Either String) Maybe (String, Int, String)
