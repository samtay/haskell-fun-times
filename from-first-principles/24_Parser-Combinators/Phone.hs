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
import Control.Monad (when)

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
parsePhone = do
  optional (try $ string "1-")
  npa <- pNPA
  _   <- optional (char '-')
  e   <- pE
  _   <- optional (char '-')
  l   <- pLN
  return $ PhoneNumber npa e l

pNPA :: Parser NumberingPlanArea
pNPA = digits 3
  <|> between (symbol "(") (symbol ")") (digits 3)

pE :: Parser Exchange
pE = digits 3

pLN :: Parser LineNumber
pLN = digits 4

digits :: (Num a, Read a) => Int -> Parser a
digits n = read <$> count n digit

-- Testing

-- This is done because Result doesn't have Show instance
toMaybe :: Result a -> Maybe a
toMaybe (Success x) = Just x
toMaybe _           = Nothing

printResult :: String -> IO ()
printResult txt = do
  let res = parseString parsePhone mempty txt
  case res of
    (Success _)    -> return ()
    (Failure info) -> print info

satisfyTest :: String -> Expectation
satisfyTest txt = (toMaybe $ parseString parsePhone mempty txt)
  `shouldBe` Just (PhoneNumber 123 456 7890)

-- Runtime

main :: IO ()
main = hspec $ do

  describe "Parsing phone number 123-456-7890" $ do
    it "parses with hyphens" $ do
      let input = "123-456-7890"
      satisfyTest input
      printResult input

    it "parses plain numbers" $ do
      let input = "1234567890"
      satisfyTest input
      printResult input

    it "parses with parentheses" $ do
      let input = "(123) 456-7890"
      satisfyTest input
      printResult input

    it "parses with 1-prefix" $ do
      let input = "1-123-456-7890"
      satisfyTest input
      printResult input
