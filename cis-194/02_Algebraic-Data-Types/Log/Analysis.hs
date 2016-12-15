module Log.Analysis
  ( parseMessage
  ) where

import Log.Base

main :: IO ()
main = putStrLn "hi"

-- Exercise 1
parseMessage :: String -> LogMessage
parseMessage = parseMWords . words

parseMWords :: [String] -> LogMessage
parseMWords ("I":t:msg)   = LogMessage Info (readInt t) $ unwords msg
parseMWords ("W":t:msg)   = LogMessage Warning (readInt t) $ unwords msg
parseMWords ("E":l:t:msg) = LogMessage (Error $ readInt l) (readInt t) $ unwords msg
parseMWords msg           = Unknown $ unwords msg

readInt :: String -> Int
readInt = read

parse :: String -> [LogMessage]
parse = (map parseMessage) . lines

-- Exercise 2
-- Not worrying about duplicates as it is not clear from exercise description.
-- We wouldn't avoid inserting distinct messages only because of their timestamps..
-- If we do want to exclude, just use equality case in compare
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree            = tree
insert msg         Leaf            = Node Leaf msg Leaf
insert (LogMessage mt ts msg) (Node left (LogMessage mtn tn msgn) right) =
  let ins = LogMessage mt ts msg
      center = LogMessage mtn tn msgn
   in case compare ts tn of 
        LT -> Node (insert ins left) center right
        _  -> Node left center (insert ins right)

-- Exercise 3
build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

-- Exercise 4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l c r) =
  (inOrder l) ++ [c] ++ (inOrder r)

-- Exercise 5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = (map extract) . (filter severe) . inOrder . build
  where severe (LogMessage (Error n) _ _) = n >= 50
        severe _                          = False
        extract (LogMessage _ _ msg)      = msg
