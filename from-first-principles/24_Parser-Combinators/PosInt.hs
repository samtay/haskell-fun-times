#!/usr/bin/env stack
{- stack runghc
  --resolver lts-7
  --install-ghc
  --package hspec
  --package trifecta
-}
module PosInt where

import Test.Hspec
import Text.Trifecta
import Control.Applicative ((<|>))

{-
Write a parser for positive integer values. Don’t reuse the pre-
existing digit or integer functions, but you can use the rest of
the libraries we’ve shown you so far. You are not expected to
write a parsing library from scratch.
-}

-- Parsing funcs

parseDigit :: Parser Char
parseDigit = oneOf ['0'..'9']

base10Integer :: Parser Integer
base10Integer = do
  ds <- some parseDigit
  return $ read ds

-- Testing

-- This is done because Result doesn't have Show instance
toMaybe :: Result a -> Maybe a
toMaybe (Success x) = Just x
toMaybe _           = Nothing

testWith :: Parser a -> String -> Maybe a
testWith p = toMaybe . parseString p mempty

-- Runtime

main :: IO ()
main = hspec $ do

  describe "Parsing per digit" $ do
    it "parses 123" $
      parseDigit `testWith` "123"
        `shouldBe` Just '1'

    it "fails abc" $
      parseDigit `testWith` "abc"
        `shouldBe` Nothing

  describe "Parsing base 10 integers" $ do
    it "parses 123abc" $
      base10Integer `testWith` "123abc"
        `shouldBe` Just 123

    it "fails abc" $
      base10Integer `testWith` "abc"
        `shouldBe` Nothing
