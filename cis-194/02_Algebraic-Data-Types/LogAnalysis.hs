module LogAnalysis
  ( parseMessage
  ) where

import Log

main :: IO ()
main = putStrLn "hi"

-- Exercise 1
parseMessage :: String -> LogMessage
parseMessage = parseMWords . words

parseMWords :: [String] -> LogMessage
parseMWords ("I":t:msg)   = LogMessage Info (readInt t) msg
parseMWords ("W":t:msg)   = LogMessage Warning (readInt t) msg
parseMWords ("E":l:t:msg) = LogMessage (Error $ readInt l) (readInt t) msg
parseMWords msg           = Unknown $ unwords msg

readInt :: String -> Int
readInt = read
