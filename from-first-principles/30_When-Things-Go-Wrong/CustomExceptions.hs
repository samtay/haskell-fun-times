module CustomExceptions where

import Control.Exception

-- Adding context
data NotEven = NotEven Int deriving (Eq, Show)
instance Exception NotEven

ensureEven :: Int -> IO Int
ensureEven i
  | odd i     = throwIO (NotEven i)
  | otherwise = return i

{-
-- Initial datatype without context
data NotEven = NotEven deriving (Eq, Show)
instance Exception NotEven

ensureEven :: Int -> IO Int
ensureEven i
  | odd i     = throwIO NotEven
  | otherwise = return i
-}
