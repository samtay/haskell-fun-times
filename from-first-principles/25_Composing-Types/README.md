# Composing types

This and the next chapter are all about *monad transformers*:
the principles behind them and the practicalities of using them.
These can make or break a great Haskeller - so I'll be thorough in notes & exercises.

Functors and applicatives are **closed under composition**,
however this is not the case for monads.

In real world Haskell, composing monads can be really convenient -
it allows us to build up computations with multiple effects.
For example, stacking IO with Monad allows us to perform IO actions while building up computations with possible failure.

A **monad transformer** is a variant of an ordinary type
that takes an additional monad type argument.
For example, MaybeT is the transformer variant of Maybe.

### 25.2 Common functions as types
First, we're going to explore the analog between functions and types, specifically `id` and `.`.
These will be used to demonstrate the non-composability of monads.
Monad transformers are conventionally written as newtypes to avoid overhead.
Note that monad transformers are never sum or product types;
they are just a means of wrapping one extra layer of monadic structure around a type.

#### Identity is boring
```haskell
newtype Identity a =
  Identity { runIdentity :: a }
```
Note the similarity between `id :: a -> a` and `Identity :: * -> *`.

#### Compose
```haskell
newtype Compose f g a =
  Compose { getCompose :: f (g a) }
  deriving (Eq, Show)
```
Note the similarity between `(.) :: (b -> c) -> (a -> b) -> a -> c`
and `Compose :: (* -> *) -> (* -> *) -> * -> *`.
Here's an example of how the type signature plays out in practice:
```haskell
value :: Compose [] Maybe Int
value = Compose [Just 1, Nothing]
```

### 25.3 Two little functors sittin’ in a tree, L-I-F-T-I-N-G
