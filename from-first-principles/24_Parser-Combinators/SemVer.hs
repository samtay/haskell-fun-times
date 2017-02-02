#!/usr/bin/env stack
{- stack runghc
  --resolver lts-7
  --install-ghc
  --package hspec
  --package trifecta
-}
module SemVer where

import Test.Hspec
import Text.Trifecta
import Control.Applicative ((<|>))
import Data.List (sort)

-- Types

data NumberOrString =
    NOSI Integer
  | NOSS String
  deriving (Eq, Ord, Show)

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]
-- NOTE: Since semver.org specifies that release identifers
-- should NOT have leading zeros,
-- but metadata CAN have leading zeros,
-- these should not be represened by the same type.
-- For example, metadata could be "001" but will be parsed to 1 :: Integer
--
-- Keeping it the same for the sake of the exercise

data SemVer =
  SemVer Major Minor Patch Release Metadata
  deriving (Eq, Show)

-- Parsing funcs

parseSemVer :: Parser SemVer
parseSemVer = do
  maj <- decimal
  _   <- char '.'
  min <- decimal
  _   <- char '.'
  pat <- decimal
  rel <- option [] $ char '-' *> someIdentifiers
  met <- option [] $ char '+' >> someIdentifiers
  return $ SemVer maj min pat rel met

someIdentifiers :: Parser [NumberOrString]
someIdentifiers = somePerDot parseNOS

somePerDot :: CharParsing m => m a -> m [a]
somePerDot p = some $ p <* skipOptional (char '.')

parseNOS :: Parser NumberOrString
parseNOS =
      -- important to use decial (not integer) and not clobber trailing chars
      NOSI <$> (try $ decimal <* notFollowedBy alphaNumDash <?> "NOSI")
  <|> NOSS <$> (some alphaNumDash)

alphaNumDash :: CharParsing m => m Char
alphaNumDash = oneOf $
     ['a'..'z']
  ++ ['A'..'Z']
  ++ ['0'..'9']
  ++ ['-','_']

-- Sorting

instance Ord SemVer where
  (SemVer majX minX patX relX _) <= (SemVer majY minY patY relY _)
    = and [majX <= majY, minX <= minY, patX <= patY]
      && ( ((not . null) relX && null relY)
        || (and (zipWith (<=) relX relY) && (length relX <= length relY)) )


-- Testing

toMaybe :: Result a -> Maybe a
toMaybe (Success x) = Just x
toMaybe _           = Nothing

testParse :: String -> Maybe SemVer
testParse = toMaybe . parseString parseSemVer mempty

sortFromString :: [String] -> Maybe [SemVer]
sortFromString vs = sort <$> traverse testParse vs

-- Runtime

main :: IO ()
main = hspec $ do

  describe "SemVer Parsing" $ do
    it "can parse first example: 2.1.1" $
      testParse "2.1.1"
        `shouldBe` Just (SemVer 2 1 1 [] [])

    it "can parse second example: 1.0.0-x.7.z.92" $
      testParse "1.0.0-x.7.z.92"
        `shouldBe` Just (SemVer 1 0 0 [NOSS "x", NOSI 7, NOSS "z", NOSI 92] [])

    describe "can parse examples from semver.org" $ do
      it "parses 1.0.0-alpha+001" $
        testParse "1.0.0-alpha+001"
          `shouldBe` Just (SemVer 1 0 0 [NOSS "alpha"] [NOSI 1])

      it "parses 1.0.0+20130313144700" $
        testParse "1.0.0+20130313144700"
          `shouldBe` Just (SemVer 1 0 0 [] [NOSI 20130313144700])

      it "parses 1.0.0-beta+exp.sha.5114f85" $
        testParse "1.0.0-beta+exp.sha.5114f85"
          `shouldBe` Just (SemVer 1 0 0 [NOSS "beta"] [NOSS "exp", NOSS "sha", NOSS "5114f85"])

  describe "SemVer Sorting" $
    it "sorts semvars correctly" $
      sortFromString
        [ "1.0.0-rc.1", "1.0.0-beta.2", "1.0.0-beta", "1.0.0-beta.11"
        , "1.0.0-alpha.1", "1.0.0-alpha", "1.0.0-alpha.beta"]
          `shouldBe` Just [ SemVer 1 0 0 [NOSS "alpha"] []
                          , SemVer 1 0 0 [NOSS "alpha", NOSI 1] []
                          , SemVer 1 0 0 [NOSS "alpha", NOSS "beta"] []
                          , SemVer 1 0 0 [NOSS "beta"] []
                          , SemVer 1 0 0 [NOSS "beta", NOSI 2] []
                          , SemVer 1 0 0 [NOSS "beta", NOSI 11] []
                          , SemVer 1 0 0 [NOSS "rc", NOSI 1] [] ]
