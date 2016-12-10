#!/usr/bin/env stack
-- stack runghc --resolver lts-7 --install-ghc --package QuickCheck
module MonoidExercises where

import SemigroupExercises hiding (main)

import Data.Monoid
import qualified Data.Semigroup as S
import Test.QuickCheck hiding (Failure, Success)
import Control.Monad

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

----------------------- 1 -----------------------
-- data Trivial = Trivial deriving (Eq, Show)

instance Monoid Trivial where
  mempty  = Trivial
  mappend = (S.<>)

----------------------- 2 -----------------------
-- newtype Identity a = Identity a deriving (Eq, Show)

instance Monoid a => Monoid (Identity a) where
  mempty  = Identity mempty
  mappend (Identity x) (Identity y) = Identity (x <> y)

----------------------- 3 -----------------------
-- data Two a b = Two a b deriving (Eq, Show)

instance (Monoid a, Monoid b) => Monoid (Two a b)
  where (Two a b) `mappend` (Two c d) = Two (a <> c) (b <> d)
        mempty = (Two mempty mempty)

----------------------- 4 -----------------------
newtype BoolConj = BoolConj Bool deriving (Eq,Show)

instance Monoid BoolConj where
  mappend (BoolConj x) (BoolConj y) = BoolConj $ x && y
  mempty                            = BoolConj True

instance Arbitrary BoolConj where
  arbitrary = arbitrary >>= return . BoolConj

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

----------------------- 5 -----------------------
newtype BoolDisj = BoolDisj Bool deriving (Eq,Show)

instance Monoid BoolDisj where
  mappend (BoolDisj x) (BoolDisj y) = BoolDisj $ x || y
  mempty                            = BoolDisj False

instance Arbitrary BoolDisj where
  arbitrary = arbitrary >>= return . BoolDisj

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

----------------------- 6 -----------------------
-- newtype Combine a b = Combine { unCombine :: (a -> b) }
instance Monoid b => Monoid (Combine a b) where
  mappend (Combine f) (Combine g) = Combine $ \x -> f x <> g x
  mempty                          = Combine $ \_ -> mempty

----------------------- 7 -----------------------
-- newtype Comp a = Comp { unComp :: (a -> a) }
instance Monoid (Comp a) where
  mappend (Comp f) (Comp g) = Comp $ f . g
  mempty                    = Comp id

----------------------- 8 -----------------------
newtype Mem s a =
  Mem { runMem :: s -> (a,s) }

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem $ \s -> (mempty,s)
  mappend (Mem f) (Mem g) =
    -- going right to left like composition
    Mem $ \s -> let (ga, gs) = g s
                    (fa, fs) = f gs
                 in (fa <> ga, fs)

f' :: (Num s) => Mem s String
f' = Mem $ \s -> ("hi", s + 1)
g' :: (Num s) => Mem s String
g' = Mem $ \s -> ("world", s + 2)
h' :: (Num s) => Mem s String
h' = Mem $ \s -> ("!!!", s + 3)

------------------- execution -------------------
main :: IO ()
main = do
  quickCheck (semigroupAssoc      :: TrivialAssoc)
  quickCheck (monoidLeftIdentity  :: Trivial -> Bool)
  quickCheck (monoidRightIdentity :: Trivial -> Bool)
  quickCheck (semigroupAssoc      :: IdentityAssoc Any)
  quickCheck (monoidLeftIdentity  :: Identity (Sum Int) -> Bool)
  quickCheck (monoidRightIdentity :: Identity All -> Bool)
  quickCheck (semigroupAssoc      :: TwoAssoc Any All)
  quickCheck (monoidLeftIdentity  :: Two All Any -> Bool)
  quickCheck (monoidRightIdentity :: Two String (Product Int) -> Bool)
  quickCheck (monoidAssoc         :: BoolConjAssoc)
  quickCheck (monoidLeftIdentity  :: BoolConj -> Bool)
  quickCheck (monoidRightIdentity :: BoolConj -> Bool)
  quickCheck (monoidAssoc         :: BoolDisjAssoc)
  quickCheck (monoidLeftIdentity  :: BoolDisj -> Bool)
  quickCheck (monoidRightIdentity :: BoolDisj -> Bool)
  putStrLn "\nTesting Mem type (match with book output)\n"
  print $ runMem (f' <> mempty) 0
  print $ runMem (mempty <> f') 0
  print $ (runMem mempty 0 :: (String, Int))
  print $ runMem (f' <> mempty) 0 == runMem f' 0
  print $ runMem (mempty <> f') 0 == runMem f' 0
  putStrLn "\nTesting associativity\n"
  print $ runMem (f' <> (g' <> h')) 0
  putStrLn "\n..should equal..\n"
  print $ runMem ((f' <> g') <> h') 0
  putStrLn "\nTesting associativity  \n"
  print $ runMem (f' <> (h' <> g')) 64
  putStrLn "\n..should equal..\n"
  print $ runMem ((f' <> h') <> g') 64
