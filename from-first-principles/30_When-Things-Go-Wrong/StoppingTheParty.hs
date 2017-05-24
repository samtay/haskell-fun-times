module StoppingTheParty where

import Control.Concurrent (threadDelay)
import Control.Exception
import Control.Monad (forever)
import System.Random (randomRIO)

tryToCatch :: Exception e => e -> IO (Either e ())
tryToCatch e = try $ throwIO e

randomException :: IO ()
randomException = do
  i <- randomRIO (1, 10 :: Int)
  if i `elem` [1..9]
    then throwIO DivideByZero
    else throwIO StackOverflow

main :: IO ()
main = forever $ do
  -- Note that a typeclass constraint does not type check here, we *need* SomeException
  let tryS :: IO () -> IO (Either SomeException ())
      tryS = try
  _ <- tryS randomException
  putStrLn "Live to loop another day!"
  -- microseconds
  threadDelay (1 * 1000000)
