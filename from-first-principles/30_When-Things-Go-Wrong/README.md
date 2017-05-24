# When things go wrong
We've seen how to *explicitly* handle error conditions via `Either` and `Maybe`,
but sometimes exceptions can be faster by eliding repeated checks for an adverse condition.

### 30.2 The Exception class and methods
Exceptions are normal types and values, but they all have an instance of the Exception typeclass:
```haskell
class (Typeable e, Show e) => Exception e where
  toException :: e -> SomeException
  fromException :: SomeException -> Maybe e
  displayException :: e -> String
```
`Typeable` defines methods for identifying types at runtime.

#### But there is something different going on here
There are many exception types defined in Control.Exception, and all are defined
totally normally except `SomeException`. It is at the heart of the `Exception` typeclass
but it also has an instance of the typeclass.

#### A brief introduction to existential quantification
`SomeException` acts as a sort of parent type for all the other exception types,
so we can handle many exceptions at once without having to match all the individual types.
```haskell
data SomeException where
  SomeException :: Exception e => e -> SomeException
```
This is a Generalized Algebraic Datatype (GADT), and the syntax is hiding something called 
*existential quantification*. Consider the equivalent definition:
```haskell
data SomeException where
  forall e . Exception e => e -> SomeException
```

Usually `forall` quantifies universally as we'd expect. However, note that the
`SomeException` *type constructor* doesn't take an argument; the type variable `e` is
a parameter to the *data constructor*. Moving the quantifier to the data constructor
limits the scope of its application and changes its meaning from *for all e* to
*there exists some e*, hence existential quantification. When the type is existentially
quantified, we can't do much with the polymorphic type variable in its data constructor.
It can't be made concrete, it must remain polymorphic, but that allows us to cram
any value that fits the typeclass constraint into that role.

Here's an example of using existential quantification just like that of `SomeException`:
```haskell
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
module WhySomeException where

import Control.Exception (ArithException(..), AsyncException(..))
import Data.Typeable

data MyException = forall e . (Show e, Typeable e) => MyException e

instance Show MyException where
  showsPrec p (MyException e) = showsPrec p e

multiError :: Int -> Either MyException Int
multiError n =
  case n of
    0 -> Left (MyException DivideByZero)
    1 -> Left (MyException StackOverflow)
    _ -> Right n
```
This allows us to return `Left e` values where `e` can inhabit two *different* types.
The first thought is, well we could just slap a typeclass constraint `e :: (Exception e) => e`
on this; but that generalizes the function too much. If we wrote
`multiError :: Exception e => Int -> Either e Int`, then returning `Left DivideByZero`
wouldn't even typecheck.

Prior to this design, we'd have to have big sum types or strings, neither of which
are extensible to structured, proper data types. But with the combination of `SomeException`
and `Typeable`, we can throw different exceptions of different types and catch
some or all of them in a handler, without having to wrap them into a parent sum type.

#### Typeable
`Typeable` allows types to be known at runtime, allowing for dynamic type checking.
It has one method:
```haskell
cast :: (Typeable a, Typeable b) => a -> Maybe b
```
this is used under the hood in the higher level functions like `fromException` and `catch`.

### 30.3 This machine kills programs
Exceptions can be thrown in pure code (like `` 2 `div` 0 ``), but can only be caught
and handled in `IO`. Since `main` is always an `IO` action, we can handle this in every program.

#### Catch me if you can
Here's a trivial example of a basic `catch :: Exception e => IO a -> (e -> IO a) -> IO a`:
```haskell
module Main where

import Control.Exception
import Data.Typeable

handler :: SomeException -> IO ()
handler (SomeException e) = do
  print (typeOf e)
  putStrLn ("We errored! It was: " ++ show e)

main =
  writeFile "zzz" "hi"
    `catch` handler
```

Note that `catch` only runs if the exception matching the type you specify in the handler is thrown.
While in this case we're just printing the error, we could obviously give any alternate `IO a`,
like writing to another file.

Here's a real live example from twitter-conduit:
```haskell
withCredentials action = do
  twinfo <- loadCredentials `catch` handleMissing
  case twinfo of
    Nothing -> getTWInfo >>= saveCredentials
    Just twinfo -> action twinfo
  where handleMissing :: IOException -> IO (Maybe TWInfo)
        handleMissing _ = return Nothing
```

### 30.4 Want either? Try!
We can lift exceptions into explicit `Either` values via
```haskell
try :: Exception e => IO a -> IO (Either e a)
```
however we are restricted to acting within IO.

Just like most other languages, exceptions in Haskell are *imprecise*.
Exception types not caught by a particular handler will roll up until explicitly caught
by another, or until it kills the program.

### 30.5 The unbearable imprecision of trying
Let's throw exceptions manually and see what we can catch:
```haskell
tryToCatch :: Exception e => e -> IO (Either ArithException ())
tryToCatch e = try $ throwIO e

-- where
throwIO :: Exception e => e -> IO a

-- repl
λ> tryToCatch DivideByZero
Left divide by zero
λ> tryToCatch StackOverflow
*** Exception: stack overflow
```
This is probably what we expected - we're only going to catch the `ArithException` that we specified.

See [StoppingTheParty](./StoppingTheParty.hs) for an example of how to catch more
than one exception, and the limitations of general `Exception` typeclass constraints.

### 30.6 Why throwIO?
There is also `throw :: Exception e => e -> a` that can be used outside of IO,
but unless writing a library it shouldn't be used, as it obscures possible effects
that should be explicit in types.
Haskell enforces that you handle exceptions in `IO` even if they were thrown
without the `IO` type; if it didn't, we'd break referential transparency!

Both `throw` and `throwIO` are often called behind the scenes in library functions.
Here's an example from http-client:
```haskell
connectionReadLine :: Connection -> IO ByteString
connectionReadLine conn = do
  bs <- connectionRead conn
  when (S.null bs) $ throwIO IncompleteHeaders
  connectionReadLineWith conn bs
```

### 30.7 Making our own exception types
A trivial example for ensuring even numbers:
```haskell
data NotEven = NotEven deriving (Eq, Show)
instance Exception NotEven

ensureEven :: Int -> IO Int
ensureEven i
  | odd i     = throwIO NotEven
  | otherwise = return i
```

But what if we want the context of how the error was caused? A well-named exception
only goes so far:
```haskell
data NotEven = NotEven Int deriving (Eq, Show)
instance Exception NotEven

ensureEven :: Int -> IO Int
ensureEven i
  | odd i     = throwIO (NotEven i)
  | otherwise = return i
```
This ensures the bad input gets included in the exception data.

If we want to also handle a custom `NotZero` exception, we can use `catches`:
```haskell
catches :: IO a -> [Handler a] -> IO a
-- where
data Handler a where
  Handler :: Exception e => (e -> IO a) -> Handler a
```
The `Handler` type uses the same existential quantification technique as `Exception`,
allowing us to specify a single typed list of handlers for different exceptions.

Of course, if our exceptions are very related, we can always define them as separate values
of a single sum type as well.

### 30.8 Surprising interaction with bottom
A tricky situation arises when we catch exceptions for values that might be bottom.
Due to non-strictness, evaluation of bottom in different places can cause implicit
unwanted behavior:
```haskell
worksFine :: IO (Either SomeException ())
worksFine = try undefined

damnBug :: IO (Either SomeException ())
damnBug = try $ return undefined

Prelude> worksFine
Left Prelude.undefined
Prelude> damnBug
Right *** Exception: Prelude.undefined
```
Non-strictness means burying the bottom in `return` causes bottom to not get forced until
evaluation is *past* the `try`; thus resulting in an uncaught error passed to `Right`.

Keep in mind:
- **Exception handling is not for catching bottoms.**
- Just squelching an exception, even `SomeException`, **doesn't mean the program won't fail**.

Unfortunately, bottoms aren't the only thing that can get past catching `SomeException`.

### 30.9 Asynchronous Exceptions
Here we'll try to kill a forked IO process:
```haskell
module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception
import System.IO

openAndWrite :: IO ()
openAndWrite = do
  h <- openFile "test.dat" WriteMode
  threadDelay 1000
  hPutStr h (replicate 100000000 '0' ++ "abc")
  hClose h

data PleaseDie = PleaseDie deriving Show
instance Exception PleaseDie

main :: IO ()
main = do
  threadId <- forkIO openAndWrite
  threadDelay 2000
  throwTo threadId PleaseDie
```

(**Note** this didn't work for me. Never got any zeroes to write to test.dat. But I
understand what the authors were demonstrating, so... anyway...) 

Asynchronous exceptions are exceptions raised from a different thread that the one
receiving the error. This is always a real threat, for example if the Haskell program
process was killed by the operating system. In some cases, like when a file handle or database 
connection is open, if possible we'd like to ignore the exception until we've had a chance
to close the connection so we can avoid resource leaks or corrupted files, etc.

Fortunately, `Control.Exception` exports `mask_`:
```haskell
-- ... same as above

main :: IO ()
main = do
  threadId <- forkIO (mask_ openAndWrite)
  threadDelay 2000
  throwTo threadId PleaseDie
```
`mask_` masks or delays exceptions thrown to the child thread until the `openAndWrite`
action is complete. Since the end of the mask is the end of the child thread action,
the exception ends up being thrown in the main thread.
