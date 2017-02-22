module WriteTheCode
  ( module WriteTheCode
  , module Control.Monad.Trans.Reader
  , module Control.Monad.Trans.State
  ) where

import Control.Monad.Trans.Reader (ReaderT(..), Reader, reader)
import Control.Monad.Trans.State (StateT(..), State)

-- 1 & 2
-- rDec is a function that should get its argument in the context of
-- Reader and return a value decremented by one.
rDec :: Num a => Reader a a
rDec = reader (+ (-1))

-- 3 & 4
-- rShow is show, but in Reader.
rShow :: Show a => Reader a String
rShow = reader show

-- 5
-- rPrintAndInc will first print the input with a greeting, then return
-- the input incremented by one.
rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = ReaderT $ \x -> do
  putStrLn $ "Hi: " ++ show x
  return $ x + 1

-- 6
-- sPrintIncAccum first prints the input with a greeting, then puts
-- the incremented input as the new state, and returns the original
-- input as a String.
sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = StateT $ \x -> do
  let sX = show x
  putStrLn $ "Hi: " ++ sX
  return (sX, x + 1)
