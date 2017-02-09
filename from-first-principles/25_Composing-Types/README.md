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
Consider this composition of functors:
```haskell
instance (Functor f, Functor g) =>
         Functor (Compose f g) where
  fmap f (Compose fga) =
    Compose $ (fmap . fmap) f fga
```
Also note that we can nest to arbitrary levels, just using `Compose`:
```haskell
v :: Compose [] Maybe (Compose Maybe [] Integer)
v = Compose [Just (Compose $ Just [1])]
```
The functor instance can evidently generalize to further nested levels,
implemented by further composing `fmap`.
Therefore, functors are closed under composition.

### 25.4 Twinplicative
As mentioned earlier, applicatives are also closed under composition, and it is up to me to prove it!

#### Exercise time
```haskell
{-# LANGUAGE InstanceSigs #-}
instance (Applicative f, Applicative g) =>
  Applicative (Compose f g) where

    pure :: a -> Compose f g a
    pure = Compose . pure . pure

    (<*>) :: Compose f g (a -> b)
          -> Compose f g a
          -> Compose f g b
    (Compose fgab) <*> (Compose fga) = Compose $ liftA2 (<*>) fgab fga
```
This is another benefit that applicative has over monad,
even though applicative is the "weaker" of the two algebras.

### 25.5 Twonad?
Although we can compose two monads, such as `Compose $ [Just 1] :: Compose [] Maybe Int`,
the result is **not** a monad.
The problem arises from a lack of information.
When we try to write a single `bind` out of two polymorphic `bind`s, we run into trouble:
```haskell
{-# LANGUAGE InstanceSigs #-}

-- IMPOSSIBLE
instance (Monad f, Monad g) => Monad (Compose f g) where
  return = pure

  (>>=) :: Compose f g a
        -> (a -> Compose f g b)
        -> Compose f g b
  (Compose fga) >>= aCfgb = -- ???

-- We have two available binds
b1 :: Monad f => f a -> (a -> f b) -> f b
b2 :: Monad g => g a -> (a -> g b) -> g b

-- and joins
j1 :: Monad f => f (f a) -> f a
j2 :: Monad g => g (g a) -> g a

-- And we need to make a bind
b3 :: (Monad f, Monad g) => f (g a) -> (a -> f (g b)) -> f (g b)

-- or a join
j3 :: (Monad f, Monad g) => f (g (f (g a))) -> f (g a)

-- It's easier to think about joins, since they are conceptually simpler.
-- Following the technique for fmap and pure, we can try composition:
j = j2 . j1 -- DOES NOT TYPE CHECK
-- but we quickly find this doesn't work, because j1 needs a nest of the **same** monad
```
So, we can't get a monad from composing two monads out of the box - we will need some additional
construct linking them together.
This is where *monad transformers* come into play.
See [Composing Monads](http://web.cecs.pdx.edu/~mpj/pubs/RR-1004.pdf) for more information.

### 25.6 Exercises: Compose Instances
#### Compose Foldable
```haskell
-- 1 Foldable

instance (Foldable f, Foldable g) =>
  Foldable (Compose f g) where
    foldMap f (Compose fga) = (foldMap . foldMap) f fga
```

#### Compose Traversable
```haskell
-- 2 Traversable

instance (Traversable f, Traversable g) =>
  Traversable (Compose f g) where
    traverse f (Compose fga) =
      Compose
        <$> (traverse . traverse) f fga

-- where
-- traverse . traverse :: (Traversable t, Traversable s, Applicative f)
--                     => (a -> f b) -> s (t a) -> f (s (t b))
```

#### And now for something completely different
Implement Bifunctor instances for the given types, where
```haskell
class Bifunctor p where
  {-# MINIMAL bimap | first, second #-}

  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
  bimap f g = first f . second g

  first :: (a -> b) -> p a c -> p b c
  first f = bimap f id

  second :: (b -> c) -> p a b -> p a c
  second = bimap id
```
Solutions are [here](./Bifunctor.hs).

### 25.7 Monad transformers
A monad transformer is a type constructor that takes a Monad as an argument and returns another Monad.

In order to compose monads,
we need to reduce the polymorphism (or, generality)
and get concrete information about one of the Monads we're working with.
The types are the trickiest part in all of this.

#### Monadic stacking
