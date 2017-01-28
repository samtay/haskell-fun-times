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
