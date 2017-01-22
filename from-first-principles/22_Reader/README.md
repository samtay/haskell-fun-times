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
```
