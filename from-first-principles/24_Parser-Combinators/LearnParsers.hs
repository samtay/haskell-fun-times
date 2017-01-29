module LearnParsers where

import Text.Trifecta
import Text.Parser.LookAhead
import Control.Applicative ((<|>))

one = char '1'

stop = unexpected "stop"

one' = one >> stop

-- read two characters, '1', and '2'
oneTwo = char '1' >> char '2'

-- read two characters, '1' and '2', then die
oneTwo' = oneTwo >> stop

-- Exercises: Parsing Practice

-- 1. Use `eof` from Text.Parser.Combinator to make the `one` and `oneTwo` parsers fail
-- for not exhausting the input stream.
ex1_one :: Parser ()
ex1_one = one >> eof

ex1_oneTwo :: Parser ()
ex1_oneTwo = oneTwo >> eof

ex1_oneTwoThree :: Parser ()
ex1_oneTwoThree = oneTwo >> char '3' >> eof

-- 2. Use `string` to make a Parser that parses "1", "12", and "123" out of the example input.
-- Try combining it with `stop` too.
-- That is, a single parser should be able to parse all three of those strings.

ex2 :: Parser [String]
-- these all look for "112123"
-- ex2 = traverse string ["1", "12", "123"]
-- ex2 = traverse (try . string) ["1", "12", "123"]
-- ex2 = sequenceA $ string <$> ["1", "12", "123"]
-- goddamn finally
-- ex2 = do
--   r1 <- lookAhead $ string "1"
--   r2 <- lookAhead $ string "12"
--   r3 <- lookAhead $ string "123"
--   return [r1, r2, r3]
--  cleanup
ex2 = traverse (lookAhead . string) ["1", "12", "123"]

-- 3. Try writing a Parser that does what `string` does, but using `char`.
ex3 :: CharParsing m => String -> m String
ex3 = traverse char

testParse :: Show a => Parser a -> IO ()
testParse p =
  print $ parseString p mempty "123"

pNL s = putStrLn ('\n' : s)

main = do
  pNL "stop:"
  testParse (stop :: Parser Char)
  pNL "one:"
  testParse one
  pNL "one':"
  testParse (one' :: Parser Char)
  pNL "oneTwo:"
  testParse oneTwo
  pNL "oneTwo':"
  testParse (oneTwo' :: Parser Char)
  pNL "ex1_one:"
  testParse ex1_one
  pNL "ex1_oneTwo:"
  testParse ex1_oneTwo
  pNL "ex1_oneTwoThree:"
  testParse ex1_oneTwoThree
  pNL "ex2:"
  testParse ex2
