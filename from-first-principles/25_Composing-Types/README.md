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
Applicative allows us to apply functions of more than one argument within functorial structure:
```haskell
-- from functor
fmap (+1) (Just 1)

-- to applicative
(,,) <$> Just 1 <*> Just "lol" <*> Just [1, 2]
```

In a similar way, sometimes we want a `>>=` which can address more than one monad at once,
such as a web app combining Reader and IO.
We might want to perform effectul actions via IO and pass around a database connection
via Reader, and might even want more Reader instances to hold HTTP request context, etc.
So, **how do we get one big bind** over a type such as `IO (Reader String [a])`
that can leverage monadic contexts of IO, Reader, and []?

#### Doing it badly
The first naive solution is to make one-off types for each combination:
```haskell
newtype MaybeIO a =
  MaybeIO { runMaybeIO :: IO (Maybe a) }
```
but this is obviously not the best solution.
We can instead get a monad for *two* types,
as long as we know what *one* of them is.
Transformers are a way of avoiding making these "one-off" monads for every possible combination.

### 25.8 IdentityT
Just as the `Identity` type has shown us the basic essence of Functor, Applicative, and Monad,
`IdentityT` will give us a introduction to the fundamentals of monad transformers.

```haskell
-- Plain old Identity. 'a' can be something with
-- more structure, but it's not required and
-- Identity won't know anything about it.
newtype Identity a =
  Identity { runIdentity :: a }
  deriving (Eq, Show)

-- The identity monad transformer, serving only to
-- to specify that additional structure should exist.
newtype IdentityT f a =
  IdentityT { runIdentityT :: f a }
  deriving (Eq, Show)

-- Functor instances
instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance (Functor m) => Functor (IdentityT m) where
  fmap f (IdentityT fa) = IdentityT (fmap f fa)

-- Applicative instances
instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity a) = Identity (f a)

instance (Applicative m) => Applicative (IdentityT m) where
  pure x = IdentityT (pure x)
  (IdentityT fab) <*> (IdentityT fa) =
    IdentityT (fab <*> fa)

-- Finally, Monad instances
-- Remember, functor & applicative can be composed generally, for all instances
-- Monad is where our instances will need particulars to make the types fit
instance Monad Identity where
  return = pure
  (Identity a) >>= f = f a

instance Monad m => Monad (IdentityT m) where
  return = pure
  (IdentityT ma) >>= f =
    IdentityT $ ma >>= runIdentityT . f
--  IdentityT $ join (runIdentityT . f <$> ma)
--  IdentityT $ join (runIdentityT <$> (f <$> ma))
```

This shows a **great insight**:
look at the *one* spot where we needed particular IdentityT info:
`ma >>= runIdentityT . f`.
To use the abstract monad `m`'s bind, of type `m a -> (a -> m b) -> m b`,
we need to make a function with type
`runIdentityT . f :: a -> m a`
as opposed to `f :: a -> IdentityT m a` alone.
**If** we left that `f` alone,
this would end up trying to join `m (IdentityT m b)`, which doesn't work,
and is the **same problem** we discovered above when trying to compose
monads in the abstract.

### 25.9 Finding a pattern
Transformers give us a `>>=` operation over two monad instances.
As should be clear now, if we write some monad `m`,
we need to create an accompanying transformer to allow it to stack with any other monads,
and that transformer monad instance will handle dealing with the extra structure generated.
"Dealing with" that extra structure is going to look like *folding*.
In general, this folding will be similar but probably more complicated than the IdentityT case,
which just used the record accessor `runIdentityT` to fold away the structure.

The *very* general (not a hard and fast rule at all) pattern of type play looks like this:
```haskell
goingFrom :: m (n m b)
          -> m (m b)
          -> m b
          -> n m b
```
