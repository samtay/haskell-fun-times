#!/usr/bin/env stack
{- stack runghc
  --resolver lts-7
  --install-ghc 
  --package QuickCheck
  --package checkers
-}
module QuickChecking where

import Control.Applicative
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- instance exists for Int, but including here for reference
instance EqProp Integer where
  (=-=) = eq

trigger :: [ (String, String, Integer) ]
trigger = undefined

main = quickBatch $ applicative trigger 
