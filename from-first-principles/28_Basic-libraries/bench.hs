#!/usr/bin/env stack
{- stack runghc
  --resolver lts-7
  --install-ghc
  --package criterion
  --
    -O2
-}
module Main where

import Criterion.Main

-- similar to base.Report version
infixl 9 !???
_ !??? n | n < 0 = Nothing
[] !??? _        = Nothing
(x:_) !??? 0     = Just x
(_:xs) !??? n    = xs !??? (n - 1)

-- similar to base non-Report version
-- not much better just like this
infixl 9 !??
{-# INLINABLE (!??) #-}
xs !?? n
  | n < 0 = Nothing
  | otherwise =
    foldr (\x r k -> case k of
                       0 -> Just x
                       _ -> r (k-1)) (const Nothing) xs n

-- now should perform like non-Report version
-- all we needed was a type signature specifying Int
-- leaving blank or specifying Integer is *way* slower
infixl 9 !?
{-# INLINABLE (!?) #-}
(!?) :: [a] -> Int -> Maybe a
xs !? n
  | n < 0 = Nothing
  | otherwise =
    foldr (\x r k -> case k of
                       0 -> Just x
                       _ -> r (k-1)) (const Nothing) xs n

list :: [Int]
list = [1..9999]

main :: IO ()
main = defaultMain
  [ bench "!! list 9999" $ whnf (list !!) 9998
  , bench "!??? list 9999" $ whnf (list !???) 9998
  , bench "!?? list 9999" $ whnf (list !??) 9998
  , bench "!? list 9999" $ whnf (list !?) 9998
  ]
