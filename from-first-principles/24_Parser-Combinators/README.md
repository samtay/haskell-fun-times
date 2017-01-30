# Parser Combinators
This chapter doesn't go too in depth into the demonstrated parsing libraries,
or even parsing itself really.
It's just a short practical introduction to enable use of Haskell's parsing libraries.

### 24.3 Understanding the parsing process
A **parser** is a function that takes some textual input (String, ByteString, Text, etc.)
and returns some **structure** as an output.

A **parser combinator** is a higher-order function that takes parsers as input
and returns a new parser as output.
(Recall combinators from the lambda calculus: *combinators* are expressions with no free variables.)
Usually the argument passing is elided,
as the interface for parsers will often be like the State monad, with implicit argument passing.
Combinators allow for parsing data according to complex ruels by gluing together parsers in a modular fashion.

#### The parsing process
The basic idea behind a parser is that you're moving a cursor around a linear stream of text,
and as we progressively build up parsing,
we'll think in *chunkier* terms than character by character.

One of the hardest problems in writing parsers is expressing things in a human-friendly way,
while maintaining performance.

The following examples use the [trifecta](http://hackage.haskell.org/package/trifecta-1.5.2) library.
Let's parse a single character, and then die using `unexpected`:
```haskell
module LearnParsers where

import Text.Trifecta

one :: CharParsing m => m Char
one = char '1'

stop :: Parser a
stop = unexpected "stop"

run = one >> stop
```
Here `unexpected` is a way of throwing an error in a parser.
Since we're sequencing via `>>`, we are throwing out the result from `one`,
yet any *effect* upon the monadic context remains.
In other words,
the result value of the parse function is thrown away,
but the effect of "moving the cursor", or "failure" remains.

So this is a bit like... State. Plus failure. As it turns out...
```haskell
type Parser a = String -> Maybe (a, String)
```
The idea is:

1. Await a string value
2. Produce result which may fail
3. Return a tuple of the desired value and whatever's left of the string

Check out some [demonstrations](./LearningParsers.hs).

#### Exercises Parsing Practice
1. Use `eof` from Text.Parser.Combinator to make the `one` and `oneTwo` parsers fail
for not exhausting the input stream.

  ```haskell
  -- Exercises: Parsing Practice

  ex1_one :: Parser ()
  ex1_one = one >> eof

  ex1_oneTwo :: Parser ()
  ex1_oneTwo = oneTwo >> eof

  ex1_oneTwoThree :: Parser ()
  ex1_oneTwoThree = oneTwo >> char '3' >> eof
  ```

2. Use `string` to make a Parser that parses "1", "12", and "123" out of the example input.
Try combining it with `stop` too.
That is, a single parser should be able to parse all three of those strings.

  ```haskell
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
  ```

3. Try writing a Parser that does what `string` does, but using `char`.

  ```haskell
  ex3 :: CharParsing m => String -> m String
  ex3 = traverse char
  ```

### 24.4 Parsing fractions
Look how simple it is to create a fraction parser!
```haskell
{-# LANGUAGE OverloadedStrings #-}
module Text.Fractions where

import Control.Applicative
import Data.Ratio ((%))
import Text.Trifecta

parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  return (numerator % denominator)

virtuousFraction :: Parser Rational
virtuousFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cannot be zero"
    _ -> return (numerator % denominator)
```
The `virtuousFraction` handles a 0 denominator error by using the monad `fail` function,
which is how we indicate parsing errors in trifecta parsing,
and allows us to handle our errors in the type system.
The initial `parseFraction` would crash when parsing "1/0" which is amateur hour esque.

#### Exercise: Unit of Success
Modify the parser `integer >> eof` to parse the same content (i.e., parse string that ends in integer) but returns the integer instead of `()`.
```haskell
-- original
Prelude> parseString (integer >> eof) mempty "123abc"
Failure (interactive):1:4: error: expected: digit,
end of input
123abc<EOF>
^

Prelude> parseString (integer >> eof) mempty "123"
Success ()

-- solution
inteof :: Parser Integer
inteof = do
  x <- integer
  eof
  return x

-- or more succinctly (discovered in next section)
inteof = integer <* eof
```

### 24.5 Haskell's parsing ecosystem
Haskell has several other excellent parsing libraries:

- parsec - popular
- attoparsec - popular
- megaparsec
- aeson - for json
- cassava - for csv

We're using trifecta in this chapter because it has great error messages.
If doing parsing in production, where speed is paramount, `attoparsec` is a good option.

#### Typeclasses of Parsers
The `Parsing` typeclass has `Alternative` as a superclass and is provided for functionality needed to describe parsers independent of input type.
Minimally, we must define `try`, `(<?>)`, and `notFollowedBy`:
```haskell
-- Text.Parser.Combinators
class Alternative m => Parsing m where
  {-# MINIMAL try, (<?>), notFollowedBy #-}
  try :: m a -> m a
  (<?>) :: m a -> String -> m a
  notFollowedBy :: Show a => m a -> m ()
  skipMany :: m a -> m ()
  skipSome :: m a -> m ()
  unexpected :: String -> m a
  eof :: m ()
```

1. `try`: takes a parser that may consume input, and on failure goes back to where it started, and fails if we didn't consume input.
2. `notFollowedBy`: does not consume input, but allows us to specify that a successful match is one *not followed by* other input.

  ```haskell
  λ> print $ parseString (integer <* notFollowedBy  eof) mempty "123abc"
  Success 123
  λ> print $ parseString (integer <* notFollowedBy  eof) mempty "123"
  Failure (..)
  ```

3. `unexpected`: signals error
4. `eof`: only succeeds at end of input

The library also defines `CharParsing`, which has `Parsing` as a superclass, and exists to parse individual characters.
```haskell
-- Text.Parser.Char
class Parsing m => CharParsing (m :: * -> *) where
  char :: Char -> m Char
  notChar :: Char -> m Char
  anyChar :: m Char
  string :: String -> m String
  text :: Text -> m Text
  satisfy :: (Char -> Bool) -> m Char
```

1. `char`: parses and returns a single character equal to the one provided
2. `notChar`: parses and returns any single character not equal to the one provided
3. `anyChar`: succeeds for any character, returns the character parsed
4. `string`: parses a sequence of characters, returns the string parsed
5. `text`: parses sequence of characters represented by Text value, returns parsed Text fragment

A reminder: this barely scratches the surface of these libraries.
Some documentation spelunking is in order.

### 24.6 Alternative
