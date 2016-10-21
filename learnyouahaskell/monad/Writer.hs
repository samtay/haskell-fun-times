module Monads.Writer (
    gcd,
    gcd',
    gcdReverse
    ) where

import Data.Monoid
import Control.Monad.Trans.Writer

-- Keeping a "log" context
isBigGang' :: Int -> Bool
isBigGang' x = x > 9

isBigGang :: Int -> (Bool, String)
isBigGang x = (x > 9, "Compared gang size to 9.")

-- But what about chaining and feeding contextual values (x, log) into this function?
-- >>= of course
applyLog' :: (a,String) -> (a -> (b,String)) -> (b,String)
applyLog' (x,log) f = let (y,newLog) = f x in (y,log ++ newLog)

-- applyLog can be made more general, anything that accepts binary (++)
applyLog'' :: (a,[c]) -> (a -> (b,[c])) -> (b,[c])
applyLog'' (x,log) f = let (y,newLog) = f x in (y,log ++ newLog)

-- the notion of appending is even more general. lets extend this to all monoids !
applyLog :: (Monoid m) => (a,m) -> (a -> (b,m)) -> (b,m)
applyLog (x,log) f = let (y,newLog) = f x in (y,log `mappend` newLog)

-- now we can stop thinking about (value, log) tuple, and just as a value with accompanying monoid
type Food = String
type Price = Sum Int -- recall Sum is a newtype for Int implementing a monoid with "sum" binary
addDrink :: Food -> (Food, Price)
addDrink "beans" = ("milk", Sum 25)
addDrink "jerky" = ("whiskey", Sum 99)
addDrink _ = ("beer", Sum 30)

----------------------------------------------------------------------
-- Writer

{--

newtype Writer w a = Writer { runWriter :: (a, w) }

instance (Monoid w) => Monad (Writer w) where
return x = Writer (x, mempty)
(Writer (x,v)) >>= f = let (Writer (y, v')) = f x in Writer (y, v `mappend` v')

--}

logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got number: " ++ show x])

multWithLog :: Writer [String] Int
multWithLog = do
    a <- logNumber 3
    b <- logNumber 5
    tell ["Gonna multiply these two nums"]
    return (a*b)

gcd'' :: Int -> Int -> Int
gcd'' a b
    | b == 0    = a
    | otherwise = gcd'' b (a `mod` b)

-- >>> mapM_ putStrLn $ snd $ runWriter (gcd' 8 3)
--
-- 8 mod 3 = 2
-- 3 mod 2 = 1
-- 2 mod 1 = 0
-- Finished with 1
gcd' :: Int -> Int -> Writer [String] Int
gcd' a b
    | b == 0 = do
        tell ["Finished with " ++ show a]
        return a
    | otherwise = do
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
        gcd' b (a `mod` b)

-- WATCH OUT - implementation below is not efficient due to right-side application of ++
-- >>> mapM_ putStrLn $ snd $ runWriter (gcdReverse 8 3)
--
-- Finished with 1
-- 2 mod 1 = 0
-- 3 mod 2 = 1
-- 8 mod 3 = 2
gcdReverse :: Int -> Int -> Writer [String] Int
gcdReverse a b
    | b == 0 = do
        tell ["Finished with " ++ show a]
        return a
    | otherwise = do
        result <- gcdReverse b (a `mod` b)
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
        return result


