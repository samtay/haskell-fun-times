#!/usr/bin/env stack
{- stack runghc
  --resolver lts-7
  --install-ghc
  --package QuickCheck
  --package checkers
-}
module QuickChecking where

import Test.QuickCheck.Checkers (quickBatch)
import Test.QuickCheck.Classes (traversable)
import Data.Monoid (Sum)

type TI = []

trigger :: TI (String, Int, String)
trigger = undefined

main = quickBatch $ traversable trigger
