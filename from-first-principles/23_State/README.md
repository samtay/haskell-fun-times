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
Let's use this and `random` to generate die ([source](./RandomExampleV2.hs)):
```haskell
module RandomExampleV2 where

import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State
import System.Random

data Die =
    One
  | Two
  | Three
  | Four
  | Five
  | Six
  deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n =
  case n of
    1 -> One
    2 -> Two
    3 -> Three
    4 -> Four
    5 -> Five
    6 -> Six
    x -> error $ "intToDie got non 1-6 integer: " ++ show x

{-
type State s = StateT s Identity
state :: Monad m => (s -> (a, s)) -> StateT s m a
      ::            (s -> (a, s)) -> State s a
-}
rollDie' :: State StdGen Die
rollDie' = state $ do
  (n, s) <- randomR (1,6)
  return (intToDie n, s)

-- Nice! Fmapping into the Functor (State StdGen :: * -> *)
rollDie :: State StdGen Die
rollDie = intToDie <$> state (randomR (1,6))

-- Nice! Lifting into the Applicative (State StdGen :: * -> *)
rollDieThrice :: State StdGen (Die, Die, Die)
rollDieThrice = liftA3 (,,) rollDie rollDie rollDie

-- This results in a list of all the same values! We're repeating a single die value
infiniteDie :: State StdGen [Die]
infiniteDie = repeat <$> rollDie

-- We need to repeat the action that produces a die.
-- replicateM :: Monad m => Int -> m a -> m [a]
nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie

-- Keeping rolling die until we reach 20
rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty = go 0 0
  where go :: Int -> Int -> StdGen -> Int
        go sum count gen
          | sum >= 20 = count
          | otherwise =
            let (die, nextG) = randomR (1, 6) gen
             in go (sum + die) (count + 1) nextG

λ> rollsToGetTwenty (mkStdGen 0)
5
λ> rollsToGetTwenty (mkStdGen 10)
6
λ> rollsToGetTwenty . mkStdGen <$> randomIO
6
λ> rollsToGetTwenty . mkStdGen <$> randomIO
5
```

#### Exercises: Roll Your Own
```haskell
-- 1. Refactor rollsToGetTwenty into having the limit be a function argument.
rollsToGetN :: Int -> StdGen -> Int
rollsToGetN n = go 0 0
  where go :: Int -> Int -> StdGen -> Int
        go sum count gen
          | sum >= n = count
          | otherwise =
            let (die, nextG) = randomR (1, 6) gen
             in go (sum + die) (count + 1) nextG

-- 2. Change rollsToGetN to recording the series of die that occurred in addition to the count.
rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged n = go 0 (0, [])
  where go :: Int -> (Int, [Die]) -> StdGen -> (Int, [Die])
        go sum (count, ds) gen
          | sum >= n = (count, ds)
          | otherwise =
            let (die, nextG) = randomR (1, 6) gen
             in go (sum + die) (count + 1, intToDie die : ds) nextG
```

### 23.6 Write State for yourself
Source [here](./Moi.hs):
```haskell
{-# LANGUAGE InstanceSigs #-}
module Moi where

newtype Moi s a =
  Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) = Moi $
    \s -> let (a, sg) = g s
           in (f a, sg)

instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi $ \s -> (a, s)
  (<*>) :: Moi s (a -> b)
        -> Moi s a
        -> Moi s b
  (Moi f) <*> (Moi g) = Moi $
    \s -> let (fab, sf) = f s
              (a, sg)   = g sf
           in (fab a, sg)

instance Monad (Moi s) where
  return = pure
  (>>=) :: Moi s a
        -> (a -> Moi s b)
        -> Moi s b
  (Moi f) >>= g = Moi $
    \s -> let (a, sf) = f s
           in runMoi (g a) sf
```

### 23.8 Chapter Exercises
Source [here](./Moi.hs):
```haskell
-- 1
get :: Moi s s
get = Moi $ \s -> (s,s)

-- 2
put :: s -> Moi s ()
put s = Moi $ const ((), s)

-- 3
exec :: Moi s a -> s -> s
exec (Moi sa) s = snd $ sa s

-- 4
eval :: Moi s a -> s -> a
eval (Moi sa) s = fst $ sa s

-- 5
modify :: (s -> s) -> Moi s ()
modify f = Moi $ \s -> ((), f s)
```
