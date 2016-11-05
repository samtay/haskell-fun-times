module Prob where
import Control.Applicative
import Control.Monad (liftM, ap)
import Data.Ratio -- 1%5 == 1/5
-- Lets try to add probabilities to the nondeterminism of lists
-- so that 3 has 50% chance of happening, 5,9 have 25% in [3,5,9]
-- [ (3,0.5), (5,0.25), (9,0.25) ]

newtype Prob a = Prob { getProb :: [(a,Rational)] } deriving Show

instance Functor Prob where
    fmap f (Prob xs) = Prob $ map (\(x,p) -> (f x,p)) xs
instance Applicative Prob where
    pure = return
    (<*>) = ap

-- To make Prob into a monad, >>= is tricky to think about, however join (fmap f m) is not so bad.
-- Join inner probabilities as we would in math, by multiplying:
-- Prob [
--      (Prob [('a', 1%2), ('b', 1%2)], 1%4),
--      (Prob [('c', 1%2), ('d', 1%2)], 3%4),
--      ] =
--      Prob [('a', 1%8), ('b', 1%8), ('c', 3%8), ('c', 3%8)]

flatten :: Prob (Prob a) -> Prob a
flatten (Prob xs) = Prob $ concat $ map multAll xs
    where multAll (Prob ys,p) = map (\(x,r) -> (x,p*r)) ys

instance Monad Prob where
    return x = Prob [(x, 1%1)]
    m >>= f = flatten (fmap f m)
    fail _ = Prob []

data Coin = Heads | Tails deriving (Show, Eq)

coin :: Prob Coin
coin = Prob [(Heads, 1%2), (Tails, 1%2)]

trickCoin :: Prob Coin
trickCoin = Prob [(Heads, 1%10), (Tails, 9%10)]

-- Flip 2 normal coins and 1 trick coin -- what are the chances all are tails?
flipThree :: Prob Bool
flipThree = do
    a <- coin
    b <- coin
    c <- trickCoin
    return (all (==Tails) [a,b,c])
