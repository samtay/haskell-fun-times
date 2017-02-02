#!/usr/bin/env stack
{- stack runghc
  --resolver lts-7
  --install-ghc
  --package hspec
  --package trifecta
-}
module Phone where

import Test.Hspec
import Text.Trifecta
import Control.Applicative ((<|>))

{-
-- 4 --
Write a parser for US/Canada phone numbers with varying
formats.
-}

-- Types

type NumberingPlanArea = Int -- aka area code
type Exchange = Int
type LineNumber = Int

data PhoneNumber =
  PhoneNumber NumberingPlanArea Exchange LineNumber
  deriving (Eq, Show)

-- Parsing funcs

parsePhone :: Parser PhoneNumber
parsePhone = undefined

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

  describe "Parsing phone number 123-456-7890" $ do
    it "parses with hyphens" $
      parsePhone `testWith` "123-456-7890"
        `shouldBe` testNumber

    it "parses plain numbers" $
      parsePhone `testWith` "1234567890"
        `shouldBe` testNumber

    it "parses with parentheses" $
      parsePhone `testWith` "(123) 456-7890"
        `shouldBe` testNumber

    it "parses with 1-prefix" $
      parsePhone `testWith` "1-123-456-7890"
        `shouldBe` testNumber

    where testNumber = Just (PhoneNumber 123 456 7890)
