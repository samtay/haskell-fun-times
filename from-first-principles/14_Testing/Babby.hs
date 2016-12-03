module Babby where

import Test.QuickCheck
import Test.QuickCheck.Gen (oneof)

------------------------- Trivial -------------------------

data Trivial =
  Trivial
  deriving (Eq, Show)

trivialGen :: Gen Trivial
trivialGen = return Trivial

instance Arbitrary Trivial where
  arbitrary = trivialGen

-- >>> sample trivialGen
-- Trivial
-- Trivial
-- Trivial
-- Trivial
-- Trivial
-- Trivial
-- Trivial
-- Trivial
-- Trivial
-- Trivial
-- Trivial

------------------------- Identity Crisis -------------------------

data Identity a =
  Identity a
  deriving (Eq, Show)

identityGen :: Arbitrary a => Gen (Identity a)
identityGen = do
  a <- arbitrary
  return (Identity a)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = identityGen

identityGenInt :: Gen (Identity Int)
identityGenInt = identityGen

data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

-- equal odds for each
sumGenEqual :: (Arbitrary a,
                Arbitrary b) =>
               Gen (Sum a b)
sumGenEqual = do
  a <- arbitrary
  b <- arbitrary
  oneof [return $ First a,
         return $ Second b]

-- >>> sample (sumGenEqual :: Gen (Sum Char Int))
-- Second 0
-- Second (-1)
-- First '\228'
-- Second 0
-- First '\213'
-- Second (-7)
-- First '^'
-- Second 13
-- Second (-7)
-- Second 2
-- First '\200'

-- Now with more interesting probabilities

sumGenFirstPls :: (Arbitrary a,
                   Arbitrary b) =>
                  Gen (Sum a b)
sumGenFirstPls = do
  a <- arbitrary
  b <- arbitrary
  frequency [(10, return $ First a),
             (1, return $ Second b)]

sumGenCharIntFirst :: Gen (Sum Char Int)
sumGenCharIntFirst = sumGenFirstPls

-- λ> sample sumGenCharIntFirst
-- First ')'
-- First '\202'
-- First 'x'
-- First '\\'
-- First '\157'
-- First '\ETB'
-- Second 11
-- First '0'
-- First '\CAN'
-- First '\231'
-- First 'C'
