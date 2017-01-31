{-# LANGUAGE QuasiQuotes #-}
module AltParsing where

import Control.Applicative
import Text.RawString.QQ
import Text.Trifecta

type NumberOrString = Either Integer String

a = "blah"
b = "123"
c = "123blah789"

eitherOr :: String
eitherOr = [r|
123
abc
456
def
|]

-- This fails for eitherOr due to leading "\n".
parseNos :: Parser NumberOrString
parseNos =
      (Left <$> integer)
  <|> (Right <$> some letter)

-- This fails for eitherOr due to trailing "\n".
parseNos'' :: Parser NumberOrString
parseNos'' =
  skipMany (oneOf "\n")
  >>
      (Left <$> integer)
  <|> (Right <$> some letter)

-- This works
parseNos''' :: Parser NumberOrString
parseNos''' = do
  skipMany (oneOf "\n")
  v <- (Left <$> integer) <|> (Right <$> some letter)
  skipMany (oneOf "\n")
  return v

-- This works by tokenization, which by default optionally consumes trailing whitespace.
parseNos' :: Parser [NumberOrString]
parseNos' = some $ token parseNos''

main = do
  print $ parseString (some letter) mempty a
  print $ parseString integer mempty b
  print $ parseString parseNos mempty a
  print $ parseString parseNos mempty b
  print $ parseString (many parseNos) mempty c
  print $ parseString (some parseNos) mempty c
  print $ parseString parseNos' mempty eitherOr
