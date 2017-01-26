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

rollDie :: State StdGen Die
rollDie = undefined
