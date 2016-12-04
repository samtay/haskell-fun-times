module QuickCheckTesting where

import Test.QuickCheck

import Data.List (sort)
import Data.Char (toUpper)

import Cipher

-- Exercise 1
half :: Fractional a => a -> a
half x = x / 2

halfIdentity :: Double -> Double
halfIdentity = (*2) . half

prop_halfIdentity :: Double -> Bool
prop_halfIdentity x = x == halfIdentity x

-- Exercise 2

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
  snd $ foldr go (Nothing, True) xs
    where go _ status@(_, False) = status
          go y (Nothing, t) = (Just y, t)
          go y (Just x, _) = (Just y, x >= y)

prop_listOrdered :: (Ord a) => [a] -> Bool
prop_listOrdered xs = listOrdered (sort xs)

prop_reversability :: (Eq a) => [a] -> Bool
prop_reversability xs = xs == (reverse . reverse) xs

-- Idempotence

twice :: (b -> b) -> b -> b
twice f = f . f

fourTimes :: (b -> b) -> b -> b
fourTimes = twice . twice

capitalizeWord :: String -> String
capitalizeWord "" = ""
capitalizeWord (c:cs) = toUpper c : cs

prop_idempotent :: (Eq a) => (a -> a) -> a -> Bool
prop_idempotent f x = all (\g -> f x == g f x) [twice, fourTimes]

prop_idempotentCapitalize :: String -> Bool
prop_idempotentCapitalize = prop_idempotent capitalizeWord

prop_idempotentSort :: [Int] -> Bool
prop_idempotentSort = prop_idempotent sort


-- Make a Gen random generator for new datatype
data Fool =
    Fulse
  | Frue
  deriving (Eq, Show)

-- 1. Equal probabilities for each.
genFool :: Gen Fool
genFool = elements [Fulse, Frue]

-- 2. 2/3s chance of Fulse, 1/3 chance of Frue.
genFool' :: Gen Fool
genFool' = frequency [ (1, return Frue)
                     , (2, return Fulse)]

-- Test Cipher
prop_encodeDecode :: (Int,String) -> Bool
prop_encodeDecode (n,word) = word == ((ceaser n) . (unCeaser n)) word


-- runtime

runQc :: IO ()
runQc = do 
  quickCheck prop_halfIdentity
  quickCheck (prop_listOrdered :: [Char] -> Bool)
  quickCheck (prop_listOrdered :: [Int] -> Bool)
  quickCheck (prop_reversability :: [Int] -> Bool)
  quickCheck prop_idempotentCapitalize
  quickCheck prop_idempotentSort
  quickCheck prop_encodeDecode
