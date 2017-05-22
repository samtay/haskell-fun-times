# IO
IO is not as complicated as most explanations suggest. In short, it is a special kind
of datatype that inherently disallows certain types of sharing, thus preventing the
effects of certain functions from percolating throughout a program (similar to the 
mutable vector ST from the previous chapter).

### 29.2 Where IO explanations go astray
Of course, IO is more than just a monad. To be precise, however, IO is an instance of
the ST monad:
```haskell
λ> :info IO
newtype IO a
  = GHC.Types.IO (GHC.Prim.State# GHC.Prim.RealWorld
                  -> (# GHC.Prim.State# GHC.Prim.RealWorld, a #))
  	-- Defined in ‘GHC.Types’
```
But thinking of IO in terms of State isn't all that helpful, since you don't
really interact with the underlying State# in IO. The State is just a signalling
mechanism telling GHC what order your IO actions are in and what a unique IO
action is. Its type parameter is only there to keep different state threads separate.
In fact, the RealWorld primitive above is not represented by anything - it uses
zero bits of memory and is erased at compile time.

Anyway, since we can't actually interact with the State on which IO is built,
it doesn't really do us any good to think about IO in terms of State.

### 29.3 The reason we need this type
Instead of considering how IO is defined, let's consider its motivation for us as
programmers. IO primarily exists to let us order operations and disable some of the
sharing mentinoned in the [Non-Strictness](../27_Non-strictness) chapter. GHC is
usually free to reorder lots of operations, delaying evaluation, sharing values,
duplicating code via inlining, among other optimizations. The **main** part of the
IO type is that it turns off these abilities.

GHC's implicit reordering is disabled in IO (as in ST) by enclosing IO actions
within nested lambdas (the only way to ensure order within a pure lambda calculus).

### 29.4 Sharing
Usually we are assured that if a function is evaluated at all, it will result in a certain type;
with the IO type, you are not guaranteed anything. So values of type `IO a` are not an `a`,
but a description of how you might get an `a` from the real world, possibly performing
effects on the way. Thus in this environment, where we don't have a value `a` but only
a means of getting a value `a`, it doesn't make sense for that value to be shared.

Consider a current time function:
```haskell
-- from Data.Time.Clock
getCurrentTime :: IO UTCTime
```
of course this couldn't work if we shared the result but needed to check the time twice.

Keep in mind the disabling of sharing is within IO but not *everywhere* within IO.
This is fairly obvious when you realize that all Haskell functions contained within the
outer `main :: IO ()` type. To look at exactly where this does and doesn't happen,
let's consider MVar.

The MVar type is a means of synchronizing shared data in Haskell. It can hold one value at a time.
Consider the following code:
```haskell
module WhatHappens where

import Control.Concurrent

myData :: IO (MVar Int)
myData = newEmptyMVar

main :: IO ()
main = do
  mv <- myData
  putMVar mv 0
  mv' <- myData
  zero <- takeMVar mv'
  print zero
```
Running this we get locked up:
```
*** Exception: thread blocked indefinitely in an MVar operation
```
because `newEmptyMVar :: IO MVar a` is a *recipe* for producing empty MVars,
not a reference to a single shared MVar. So the two `myData` references are **not**
referring to the same MVar. And when you try to take a value out of an empty MVar,
the program is blocked until something is put into that MVar, hence the deadlock exception.

The point is, when you see a type like `IO String` or `IO (MVar a)`, you don't have a reference
to a single string or shared MVar; you have a recipe for producing a string or an empty MVar.

### 29.5 IO doesn't disable sharing for everything
As mentioned earlier, IO obvoiusly doesn't disable sharing for everything.
It disables sharing for the **terminal values** it reduces to, such as the `MVar a` in `IO MVar a`.
Values not dependent on IO for evaluation can still be shared:
```haskell
woot :: IO String
woot = return (trace "evaluted woot" "woot")

main :: IO ()
main = do
  w <- woot
  putStrLn w
  putStrLn w

Prelude Debug.Trace
λ> main
woot evaluated
woot
woot
```
notice that sharing is not disabled for values not in IO that happen to be used in the course
of running an IO action.

### 29.6 Purity is losing meaning
The interpretation of the words "purely functional" have changed over the years.
The phrase used to describe a language whose semantics would be only lambda calculus.
Impure functional languages were more typical, augmenting the calculus to allow for imperitave,
effectful programs.

The mistake poeple make with IO is conflating effects with semantics. We don't lose referential
transparency with the effects allowed in Haskell. A function that returns `IO a` is still
referentially transparent, because given the same arguments, it will generate the same IO action
every time. A function `f :: Bool -> IO [Int]` that returns an action which produces
10 random integers might produce different literal values, but it's still returning the same *action*.
Referential transparency is preserved because we're returning the same *IO action* or recipe.

### 29.7 IO's Functor, Applicative, and Monad
Another common mistake is the "IO is a monad" perspective. Sure it is, but the better
perspective is "IO is a datatype that has a Monad instance, along with Functor and
Applicative instances." Since IO is a unique type, let's explicitly define what each instance
accomplishes.

#### The IO Functor
`fmap :: (a -> b) -> IO a -> IO b` lifts a function over the effects that we
might perform to obtain the `a` value. In other words, it produces a new IO action `IO b` in terms
of the old action `IO a` by transforming the final result of the `IO a` action.

#### The Applicative IO
Similar to the `fmap` concept, `(<*>) :: IO (a -> b) -> IO a -> IO b` takes a recipe for producing
a function `f :: a -> b` and a recipe for producing an `a` value, and returns a recipe for
performing `f` on the result of the recipe for `a`; hence returning `IO b`.

#### Monad and IO
Think of `pure`/`return` as an effect-free way to embed a value in a recipe-creating environment.
The monad instance differs from the applicative in that effects performed by an outer IO action
can influence what recipe we get from the inner portion:
```haskell
module NestedIO where

import Data.Time.Calendar
import Data.Time.Clock
import System.Random

huehue :: IO (Either (IO Int) (IO ()))
huehue = do
  t <- getCurrentTime
  let (_, _, dayOfMonth) = toGregorian (utctDay t)
  case even dayOfMonth of
    True  -> return $ Left randomIO
    False -> return $ Right (putStrLn "no soup for you")
```
here the `IO` returned is dependent on effects from an outer `IO` action, namely getting the time.
This is inexpressible with Applicative.

##### Monadic associativity
It's important to keep in mind the "recipe" perspective. The IO bind from its monad instance
is associative, even if IO actions seem to contradict this. It's because we are constructing
recipes, not executing actions. Binding over an IO action does not execute it, it produces a
*new* IO action in terms of the old one.

### 29.8 Well, then, how do we MVar?
So, the right way to refer to the MVar in the above example is:
```haskell
import Control.Concurrent
main :: IO ()
main = do
  mv <- newEmptyMVar
  putMVar mv (0 :: Int)
  zero <- takeMVar mv
  print zero
```
But we can also do this in a sinister manner via `unsafePerformIO`:
```haskell
import Control.Concurrent
import System.IO.Unsafe
myData :: MVar Int
myData = unsafePerformIO newEmptyMVar
main :: IO ()
main = do
  putMVar myData 0
  zero <- takeMVar myData
  print zero
```

Note that `unsafePerformIO :: IO a -> a`, which is definitely not a good idea, and can
actually break referential transparency, but it does allow us to accomplish implicit
sharing of the MVar wherever we want.  In real code we'd pass references to MVars around
as arguments (typically using Reader monad).

### 29.9 Chapter Exercises
1. [File I/O with Vigenere](./Vigenere/Main.hs)
2. [Config directories](./Config/Main.hs)
