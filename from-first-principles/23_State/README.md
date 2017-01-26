# State

### 23.2 What is state?
The concept of state originates in automata theory, with binary "state" of each cell value.
The definition is more broad now, and is essentially the entire stored information within a program.

In most imperative languages, statefulness is pervasive and implicit.
This is obviously forbidden in Haskell, given referential transparency;
all we can do is accept arguments and return a result.
The monadic interface for state is a convenient and clean way of handling potentially changing values
without resorting to mutability.
While the monadic interface is not *necessary*, it does allow us to have state which

1. does not require IO
2. is limited only to data in State container
3. maintains referential transparency
4. is explicit in type signatures

There are other ways to share data within a program as well;
the State datatype is appropriate when you want to express your program in terms of values that
potentially vary with each evaluation step,
which can be read and modified,
but don't otherwise have specific operational constraints.

### 23.4 The State newtype
```haskell
newtype State s a =
  State { runState :: s -> (a, s) }

-- compared to
newtype Reader r a =
  Reader { runReader :: r -> a }
```

#### Aside about isomorphism
Since newtypes have the same underlying representation as the type they wrap
(the wrapper disappears at compile time),
the function contained in the newtype must be isomorphic to the type it wraps.
This is clearly true with a function `st :: s -> (a, s)`,
as we can go into the newtype via the data constructor: `State st :: State s a`
and we can go from the newtype via the accessor: `runState (State st) :: s -> (a, s)`.
Clearly this is an isomorphism and `runState` is the inverse of `State`.

Contrast this with functions of type `a -> Maybe b`, `b -> Maybe a` -- these
are not guaranteed isomorphisms, due to the possibilty of failure.

### 23.5 Throw down
Let's use this and `random` to generate die:
```haskell
module RandomExample where

import System.Random

data Die =

```
