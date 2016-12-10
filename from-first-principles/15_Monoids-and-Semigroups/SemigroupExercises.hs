#!/usr/bin/env stack
-- stack runghc --resolver lts-7 --install-ghc --package QuickCheck
module SemigroupExercises where

import Data.Semigroup
import Test.QuickCheck hiding (Failure, Success)
import Control.Monad

-- Given a datatype, implement the Semigroup instance.
-- Add necessary contraints / instances where necessary.
-- Validate all of the instances with QuickCheck

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

----------------------- 1 -----------------------
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

----------------------- 2 -----------------------
newtype Identity a = Identity a deriving (Eq, Show)

instance (Semigroup a) => Semigroup (Identity a) where
  (Identity x) <> (Identity y) = Identity (x <> y)

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = arbitrary >>= return . Identity

type IdentityAssoc a = (Identity a) -> (Identity a) -> (Identity a) -> Bool

----------------------- 3 -----------------------
data Two a b = Two a b deriving (Eq, Show)

-- multiple options, I'll treat this as a tuple
instance (Semigroup a, Semigroup b) => Semigroup (Two a b)
  where (Two a b) <> (Two c d) = Two (a <> c) (b <> d)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b)
  where arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return $ Two a b

type TwoAssoc a b = (Two a b) -> (Two a b) -> (Two a b) -> Bool

----------------------- 8 -----------------------
data Or a b = Fst a | Snd b
  deriving (Eq, Show)
-- with the following behavior
-- Fst 1 <> Snd 2 === Snd 2
-- Fst 1 <> Fst 2 === Fst 2
-- Snd 1 <> Fst 2 === Snd 1
-- Snd 1 <> Snd 2 === Snd 1
instance Semigroup (Or a b) where
  Snd x <> Snd y = Snd x
  Fst x <> Fst y = Fst y
  Snd x <> _     = Snd x
  _ <> Snd x     = Snd x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [ (1, return $ Fst a)
              , (1, return $ Snd b) ]


type OrAssoc a b = (Or a b) -> (Or a b) -> (Or a b) -> Bool

----------------------- 9 -----------------------
newtype Combine a b =
  Combine { unCombine :: (a -> b) }
-- with the following behavior
-- >>> let f = Combine $ \n -> Sum (n+1)
-- >>> let g = Combine $ \n -> Sum (n-1)
-- >>> unCombine (f <> g) $ 0
-- Sum {getSum = 0}
-- >>> unCombine (f <> g) $ 1
-- Sum {getSum = 2}
-- >>> unCombine (f <> f) $ 1
-- Sum {getSum = 4}
-- >>> unCombine (g <> f) $ 1
-- Sum {getSum = 2}
instance Semigroup b => Semigroup (Combine a b) where
  cf <> cg = Combine $ \x -> f x <> g x
    where f = unCombine cf
          g = unCombine cg
type CombineAssoc a b = (Combine a b) -> (Combine a b) -> (Combine a b) -> Bool
-- http://www.cis.upenn.edu/~bcpierce/courses/advprog/lectures/lec18.pdf
-- taking too long to figure out how to generate random funcs, leaving for later

----------------------- 10 ----------------------
newtype Comp a = Comp { unComp :: (a -> a) }

-- Same thing as Combine - author probably looking for something else here?
instance Semigroup a => Semigroup (Comp a) where
  cf <> cg = Comp $ \x -> f x <> g x
    where f = unComp cf
          g = unComp cg

----------------------- 10 ----------------------
data Validation a b = Failure a | Success b deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
  Success x <> Success y = Success y
  Failure x <> Failure y = Failure $ x <> y
  Failure x <> _         = Failure x
  _         <> Failure x = Failure x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [ (1, return $ Failure a)
              , (1, return $ Success b) ]

type ValAssoc a b = (Validation a b) -> (Validation a b) -> (Validation a b) -> Bool

----------------------- 11 ----------------------
newtype AccumulateRight a b =
  AccumulateRight (Validation a b)
  deriving (Eq, Show)

instance Semigroup b =>
  Semigroup (AccumulateRight a b) where
    (<>) (AccumulateRight vx) (AccumulateRight vy) = AccumulateRight $ vx <<>> vy where
      Success x <<>> Success y = Success $ x <> y
      Failure x <<>> Failure y = Failure x
      Success x <<>> _         = Success x
      _         <<>> Success x = Success x

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateRight a b) where
  arbitrary = arbitrary >>= return . AccumulateRight

type AccRightAssoc a b =
  (AccumulateRight a b) -> (AccumulateRight a b) -> (AccumulateRight a b) -> Bool


----------------------- 11 ----------------------
newtype AccumulateBoth a b =
  AccumulateBoth (Validation a b)
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) =>
  Semigroup (AccumulateBoth a b) where
    (<>) (AccumulateBoth vx) (AccumulateBoth vy) = AccumulateBoth $ vx <<>> vy where
      Success x <<>> Success y = Success $ x <> y
      Failure x <<>> Failure y = Failure $ x <> y
      Failure x <<>> _         = Failure x
      _         <<>> Failure x = Failure x

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateBoth a b) where
  arbitrary = arbitrary >>= return . AccumulateBoth

type AccBothAssoc a b = (AccumulateBoth a b) -> (AccumulateBoth a b) -> (AccumulateBoth a b) -> Bool

-------------------------------------------------
-- Some necessary arbitrary instances for testing

instance (Arbitrary a) => Arbitrary (Sum a) where
  arbitrary = arbitrary >>= return . Sum
instance (Arbitrary a) => Arbitrary (Product a) where
  arbitrary = arbitrary >>= return . Product
instance Arbitrary All where
  arbitrary = arbitrary >>= return . All
instance Arbitrary Any where
  arbitrary = arbitrary >>= return . Any

------------------- execution -------------------
main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivialAssoc)
  quickCheck (semigroupAssoc :: IdentityAssoc (Product Int))
  quickCheck (semigroupAssoc :: IdentityAssoc (Sum Rational))
  quickCheck (semigroupAssoc :: IdentityAssoc String)
  quickCheck (semigroupAssoc :: TwoAssoc String All)
  quickCheck (semigroupAssoc :: TwoAssoc Any All)
  quickCheck (semigroupAssoc :: OrAssoc String All)
  quickCheck (semigroupAssoc :: OrAssoc Any All)
  quickCheck (semigroupAssoc :: ValAssoc (Sum Int) Int)
  quickCheck (semigroupAssoc :: ValAssoc All String)
  quickCheck (semigroupAssoc :: AccRightAssoc Int (Product Int))
  quickCheck (semigroupAssoc :: AccRightAssoc Double [String])
  quickCheck (semigroupAssoc :: AccBothAssoc (Sum Int) (Product Int))
  quickCheck (semigroupAssoc :: AccBothAssoc All [String])
