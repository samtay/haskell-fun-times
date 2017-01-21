#!/usr/bin/env stack
{- stack runghc
  --resolver lts-7
  --install-ghc
  --package QuickCheck
  --package checkers
-}
module QuickChecking where

import Test.QuickCheck (Arbitrary(..), frequency, listOf)
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

-- 2 Constant
newtype Constant a b = Constant a
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant x) = Constant x

instance Foldable (Constant a) where
  foldMap _ _ = mempty

instance Traversable (Constant a) where
  traverse _ (Constant x) = Constant <$> pure x

-- 3 Maybe
data Optional a = Nada | Yep a
  deriving (Eq, Ord, Show)

instance Functor Optional where
  fmap _ Nada    = Nada
  fmap f (Yep x) = Yep $ f x

instance Foldable Optional where
  foldMap _ Nada    = mempty
  foldMap f (Yep x) = f x

instance Traversable Optional where
  sequenceA Nada    = pure Nada
  sequenceA (Yep x) = Yep <$> x

-- 4 List
data List a = Nil | Cons a (List a)
  deriving (Eq, Ord, Show)

instance Functor List where
  fmap _ Nil         = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Foldable List where
  foldMap _ Nil         = mempty
  foldMap f (Cons x xs) = f x `mappend` foldMap f xs

instance Traversable List where
  traverse _ Nil         = pure Nil
  traverse f (Cons x xs) = Cons <$> f x <*> traverse f xs

-- 5 Three
data Three a b c = Three a b c
  deriving (Eq, Ord, Show)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

instance Foldable (Three a b) where
  foldMap f (Three _ _ z) = f z

instance Traversable (Three a b) where
  sequenceA (Three x y z) = Three x y <$> z

-- 6 Three'
data Three' a b = Three' a b b
  deriving (Eq, Ord, Show)

instance Functor (Three' a) where
  fmap f (Three' a x y) = Three' a (f x) (f y)

instance Foldable (Three' a) where
  foldMap f (Three' _ x y) = f x `mappend` f y

instance Traversable (Three' a) where
  sequenceA (Three' a x y) = Three' a <$> x <*> y

-- 7 S
data S n a = S (n a) a
  deriving (Eq, Ord, Show)

instance Functor n => Functor (S n) where
  fmap f (S n x) = S (fmap f n) (f x)

instance Foldable n => Foldable (S n) where
  foldMap f (S n x) = (foldMap f n) `mappend` f x

instance Traversable n => Traversable (S n) where
  traverse f (S n x) = S <$> (traverse f n) <*> f x

-- 8 Tree
data Tree a =
    Empty
  | Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
  fmap _ Empty                 = Empty
  fmap f (Leaf x)              = Leaf $ f x
  fmap f (Node leftN x rightN) = Node (fmap f leftN) (f x) (fmap f rightN)

instance Foldable Tree where
  foldMap _ Empty                 = mempty
  foldMap f (Leaf x)              = f x
  foldMap f (Node leftN x rightN) =
    mconcat
      [ foldMap f leftN
      , f x
      , foldMap f rightN ]

instance Traversable Tree where
  traverse _ Empty                 = pure Empty
  traverse f (Leaf x)              = Leaf <$> f x
  traverse f (Node leftN x rightN) =
    Node
      <$> traverse f leftN
      <*> f x
      <*> traverse f rightN

----------------- Testing stuff -----------------

-- Arbitrary and EqProp instances
instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary

instance Eq a => EqProp (Constant a b) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = frequency [ (1, Yep <$> arbitrary)
                        , (2, return Nada) ]

instance Eq a => EqProp (Optional a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = (foldr Cons Nil) <$> listOf arbitrary

instance Eq a => EqProp (List a) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b, Arbitrary c)
  => Arbitrary (Three a b c) where
    arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b)
  => Arbitrary (Three' a b) where
    arbitrary = Three' <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

instance (Arbitrary (n a), Arbitrary a) => Arbitrary (S n a) where
  arbitrary = S <$> arbitrary <*> arbitrary

instance (Eq (n a), Eq a) => EqProp (S n a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = frequency [ (2, return Empty)
                        , (1, Leaf <$> arbitrary)
                        , (1, Node <$> arbitrary <*> arbitrary <*> arbitrary) ]

instance Eq a => EqProp (Tree a) where
  (=-=) = eq

-- Trigger type
type Trigger = (String, Int, String)
trigger = undefined

-- IO runtime
main = do
  quickBatch $ traversable (trigger :: Identity Trigger)
  quickBatch $ traversable (trigger :: Constant Trigger Trigger)
  quickBatch $ traversable (trigger :: Optional Trigger)
  quickBatch $ traversable (trigger :: List Trigger)
  quickBatch $ traversable (trigger :: Three Trigger Trigger Trigger)
  quickBatch $ traversable (trigger :: Three' Trigger Trigger)
  quickBatch $ traversable (trigger :: S Maybe Trigger)
  quickBatch $ traversable (trigger :: S List Trigger)
  quickBatch $ traversable (trigger :: Tree Trigger)
