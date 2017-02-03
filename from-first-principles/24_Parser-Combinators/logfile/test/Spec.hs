module Main where

import Test.QuickCheck hiding (Result(..))
import Text.Trifecta

-- TODO quickcheck generators

-- This is done because Result doesn't have Show instance
toMaybe :: Result a -> Maybe a
toMaybe (Success x) = Just x
toMaybe _           = Nothing
-- Runtime

main :: IO ()
main = undefined
