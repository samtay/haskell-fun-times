# Reader

The Reader newtype allows us to pass around information that might be needed intermittently or universally throughout an application.

### 22.2 A new beginning
Consider the following:
```haskell
import Control.Applicative

boop = (*2)
doop = (+10)

bip :: Integer -> Integer
bip = boop . doop

bloop :: Integer -> Integer
bloop = fmap boop doop
```
`bip` and `bloop` do the same thing here.
We are fmapping `boop` over the functorial context of a function `doop`, which is equivalent to composition.
We can take this further via applicative:
```haskell
bbop :: Integer -> Integer
bbop = (+) <$> boop <*> doop

duwop :: Integer -> Integer
duwop = liftA2 (+) boop doop
```
This adds another function `(+)` to lift over the context of the functions `boop` and `doop`.
Now, an argument will get passed to both `boop` and `doop` in parallel, and the results will be passed to `(+)`:
```haskell
λ> duwop 3
19
```

To help with the mental type juggling, we can assert a more concrete type for functions like `<*>` and see if the compiler agrees in the possibility:
```haskell
λ> :t (<*>)
(<*>) :: Applicative f => f (a -> b) -> f a -> f b

λ> :t (<*>) :: (a -> a -> b) -> (a -> a) -> (a -> b)
(<*>) :: (a -> a -> b) -> (a -> a) -> (a -> b)
  :: (a -> a -> b) -> (a -> a) -> a -> b
λ>
```

This is useful when two functions share the same input and we want to apply another function to both results.
Here's a real life example, calculating if either of two predicates is satisfied by a single input:
```haskell
module Web.Shipping.Utils ((<||>)) where

import Control.Applicative (liftA2)

(<||>) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(<||>) = liftA2 (||)
```

These are the main ideas of *Reader*.
It is a way of stringing functions together when all those functions are awaiting one input from a shared environment.
A common use case is when we have a constant value that is obtained from outside of the program that will be an argument to a whole bunch of functions.
Reader allows us to avoid passing that argument around explicitly.

#### Warming Up
Given `cap` and `rev`, implement `composed`, `fmapped`, and `tupled` below.
Write `tupled` in applicative, monadic `do`, and monadic `>>=` contexts.
```haskell
import Data.Char

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = rev . cap

fmapped :: [Char] -> [Char]
fmapped = fmap rev cap

tupled :: [Char] -> ([Char], [Char])
tupled = liftA2 (,) rev cap
tupled = rev >>= (\r -> cap >>= (\c -> return (r,c)))
tupled = do
  r <- rev
  c <- cap
  return (r, c)
```

### 22.3 This is Reader
We use function composition because it lets us compose two functions without explicitly having to recognize the argument that will eventually arrive.
The *Functor* of functions is function composition;
with this functor, we can map an ordinary function `b -> c` over another function `a -> b`,
to create a new function awaiting a final argument of type `a`.
The *Applicative* and *Monad* instances for function allow us to map a function that is awaiting an argument `a` over *another* function that is awaiting an argument of type `a`.

### 22.4 Breaking down the Functor of functions
One thing to keep in mind is that the functor instance is defined for `(->) r`,
the partially applied type constructor of functions.
Similar to the functor instances for `Either a` and `(,) a`,
this means that given a function type `(->) r b`,
we are lifting over `(->) r` and only transforming the `b` value.

So the argument of the function is part of the structure being lifted over,
while the result of the function is the value being transformed.
This lines up with composition:
```haskell
(.)  :: (b -> c) -> (a -> b) -> (a -> c)
fmap :: Functor f => (b -> c) -> f b -> f c
```
It is clear that substituting `f` with `((->) a)` results in composition.

### 22.5 But uh, Reader?
Reader is a newtype wrapper for functions:
```haskell
newtype Reader r a =
  Reader { runReader :: r -> a }

instance Functor (Reader r) where
  fmap :: (a -> b) -> Reader r a -> Reader r b
  fmap f (Reader ra) = Reader $ (f . ra)
```
And we can see that fmap for Reader is basically

1. unpack `g :: r -> a` out of Reader
2. Compose `f` with  `g`
3. Put `f` . `g` back into Reader

#### Exercise: Ask
```haskell
ask :: Reader a a
ask = Reader id
```

### 22.6 Functions have an Applicative too
First, let's specialize the types:
```haskell
pure :: a -> f a
pure :: a -> (r -> a)

(<*>) :: f (a -> b) -> f a -> f b
(<*>) :: (r -> a -> b) -> (r -> a) -> (r -> b)
```

#### Demonstrating the function applicative
Given the two records:
```haskell
data Person =
  Person {
    humanName :: HumanName
  , dogName :: DogName
  , address :: Address
  } deriving (Eq, Show)

data Dog =
  Dog {
    dogsName :: DogName
  , dogsAddress :: Address
  } deriving (Eq, Show)
```
here's an example of avoiding explicitly passing around arguments:
```haskell
-- without Reader
getDog :: Person -> Dog
getDog p = Dog (dogName p) (address p)

-- with Reader
getDog :: Person -> Dog
getDog = Dog <$> dogName <*> address

-- with Reader, alternate
import Control.Applicative (liftA2)
getDog :: Person -> Dog
getDog = liftA2 Dog dogName address
```

#### Exercise: Reading Comprehension

1. Write `liftA2`.

  ```haskell
  myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
  myLiftA2 f x y = f <$> x <*> y
  ```

2. Write `asks` based on its type signature.

  ```haskell
  asks :: (r -> a) -> Reader r a
  asks = Reader
  ```

3. Implement Applicative for Reader

  ```haskell
  {-# LANGUAGE InstanceSigs #-}
  module Test where

  newtype Reader r a = Reader { runReader :: (r -> a) }

  instance Functor (Reader r) where
    fmap f (Reader ra) = Reader $ f . ra

  instance Applicative (Reader r) where
    pure :: a -> Reader r a
    pure a = Reader $ const a

    (<*>) :: Reader r (a -> b)
          -> Reader r a
          -> Reader r b
    (Reader rab) <*> (Reader ra) =
      Reader $ \r -> rab r (ra r)
  ```

### 22.7 The Monad of functions
This is a cute example to derive `>>=`, from specific to abstract:
```haskell
foo :: (Functor f, Num a) => f a -> f a
foo r = fmap (+1) r

bar :: Foldable f => t -> f a -> (t, Int)
bar r t = (r, length t)

-- combined action
frooty :: Num a => [a] -> ([a], Int)
frooty r = bar (foo r) r

-- abstracted
fooBind :: (r -> a) -> (a -> r -> b) -> (r -> b)
fooBind m k = \r -> k (m r) r
```

#### Exercise: Implement the Reader Monad
```haskell
instance Monad (Reader r) where
  return = pure
  (>>=) :: Reader r a
        -> (a -> Reader r b)
        -> Reader r b
  (Reader ra) >>= aRb =
    Reader $ \r -> runReader (aRb (ra r)) r
```

### 22.8 Reader Monad by itself is kinda boring
Because it can't do anything that the Applicative can't:
```haskell
instance Monad ((->) r) where
  return = pure
  m >>= k = flip k <*> m
```
Of course this isn't generally true, but we can take advantage of the fact that we're operating on functions (usage of `flip`).

### 22.9 You can change what comes below, but not above
The "read-only" means that within any given scope (say the body of a function `foo`),
you can swap in a different type/value for the "read-only" `r` for subsequent functions that you call,
but not for functions outside the scope, like the function calling `foo`.
It's kind of painfully obvious, given the natural immutability of Haskell.
However, in the next chapter the State monad will contrast this principle.
```haskell
outer =
  -- foo cannot change r
  foo r
  -- foo cannot change r
  other r

foo r =
  -- foo can choose to modify r for subsequence funcs
  bar (r+1)

bar r = print r
```

### 22.10 You tend to see ReaderT, not Reader
Usually it's just one Monad in a **stack** of multiple types,
such as the monad transformer ReaderT.
Also, Reader of Int is almost trivial.
Usually the type is a record of several values.

### 22.11 Chapter Exercises
Warm up questions (odd ones, not useful as notes) in [ReaderPractice.hs](./ReaderPractice.hs).
A more meaningful problem is [rewriting shawty](./shawty-prime/app/Main.hs)
to use ReaderT to make the database connection available,
instead of the [original version](https://github.com/bitemyapp/shawty-prime/blob/master/app/Main.hs)
which manually passed the database connection to the app.
