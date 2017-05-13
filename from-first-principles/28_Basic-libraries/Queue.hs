module Queue where

import Criterion.Main

data Queue a =
  Queue { enqueue :: [a]
        , dequeue :: [a]
        } deriving (Eq, Show)

empty :: Queue a
empty = Queue [] []

-- ??
push :: a -> Queue a -> Queue a
push x (Queue es ds) = Queue (x:es) ds

pop :: Queue a -> Maybe (a, Queue a)
pop (Queue [] []) = Nothing
pop (Queue es []) = pop $ Queue [] (reverse es)
pop (Queue es (x:ds)) = Just (x, Queue es ds)

fromList :: [a] -> Queue a
fromList xs = Queue [] (reverse xs)

toList :: Queue a -> [a]
toList (Queue es ds) = reverse ds ++ es

-- Performance

listTest :: Int -> [Int]
listTest i = go i [1..100]
  where go 0 xs = xs
        go n xs = go (n-1) (last xs : init xs)

queueTest :: Int -> [Int]
queueTest i = toList $ go i $ fromList [1..100]
  where go 0 xs = xs
        go n xs = let Just (x, ys) = pop xs in go (n-1) (push x ys)

main :: IO ()
main = defaultMain
  [ bench "pop/push list" $ whnf listTest 123456
  , bench "pop/push queue" $ whnf queueTest 123456
  ]
