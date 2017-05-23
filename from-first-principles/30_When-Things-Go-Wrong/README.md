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
