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
