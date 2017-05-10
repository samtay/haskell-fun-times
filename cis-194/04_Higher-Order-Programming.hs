module Main where

-- Exercise 1 : Wholemeal programming
-- Reimplement each of the following functions in a more idiomatic
-- Haskell style. Use wholemeal programming practices, breaking each
-- function into a pipeline of incremental transformations to an entire
-- data structure. Name your functions fun1’ and fun2’ respectively.
-- Hint: For this problem you may wish to use the functions
-- iterate and takeWhile. Look them up in the Prelude documentation to see
-- what they do.

-- 1 .
fun1 :: [Int] -> Int
fun1 []     = 1
fun1 (x:xs)
  | even x    = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Int] -> Int
fun1' = product . map (subtract 2) . filter even

-- 2 .
fun2 :: Int -> Int
fun2 1 = 0
fun2 n | even n    = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

fun2' :: Int -> Int
fun2' = sum
  . filter even
  . takeWhile (/=1)
  . iterate (\n -> if even n then n `div` 2 else 3 * n + 1)

testequiv :: Eq b => (a -> b) -> (a -> b) -> a -> Bool
testequiv f1 f2 = (==) <$> f1 <*> f2

main :: IO ()
main = do
  let l2 = [-2, 5, 3, 8, 9]
      l1 = [1,3,4,5]
  if all (testequiv fun1 fun1') [l1, l2]
     then putStrLn "Test fun1 == fun1' passed"
     else putStrLn "Test fun1 == fun1' failed"
  if all (testequiv fun2 fun2') l1
     then putStrLn "Test fun2 == fun2' passed"
     else putStrLn "Test fun2 == fun2' failed"

-- Exercise 2 : Folding with trees
-- Recall the definition of a binary tree data structure. The height of a
-- a binary tree is the length of a path from the root to the deepest
-- node. For example, the height of a tree with a single node is 0; the
-- height of a tree with three nodes, whose root has two children, is 1;
-- and so on. A binary tree is balanced if the height of its left and right
-- subtrees differ by no more than 1, and its left and right subtrees are
-- also balanced.
--
-- You should use the following data structure to represent binary
-- trees. Note that each node stores an extra Int representing the
-- height at that node.

data Tree a = Leaf
            | Node Int (Tree a) a (Tree a)
            deriving (Eq)

-- For this exercise, write a function which generates a balanced binary
-- tree from a list of values using foldr.

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf

insert :: a -> Tree a -> Tree a
insert x Leaf             = Node 0 Leaf x Leaf
insert x t@(Node h l a r) =
  if size l <= size r
     then Node newH (insert x l) a r -- prefer left
     else Node newH l a (insert x r)  -- then right
       where newH = if totallyBalanced t then (h+1) else h


-- Fuck thats not right
totallyBalanced :: Tree a -> Bool
totallyBalanced Leaf                 = False
totallyBalanced (Node _ Leaf _ Leaf) = True
totallyBalanced (Node _ l _ r)       = totallyBalanced l && totallyBalanced r

size :: Tree a -> Int
size Leaf = 0
size (Node _ l _ r) = 1 + size l + size r

instance Show a => Show (Tree a) where
  show Leaf = "_"
  --show (Node h Leaf a Leaf) = replicate h ' ' ++ show h ++ " - " ++ show a
  show (Node h l a r) =
       show l
    ++ "\n"
    ++ replicate ((h+1)*2) ' ' ++ show h ++ " - " ++ show a
    ++ "\n"
    ++ show r

-- Exercise 3 : More folds!
-- 1.  Implement a function xor :: [Bool] -> Bool which returns
-- True if and only if there are an odd number of True
-- values contained in the input list. It does not matter how many
-- False values the input list contains. For example,
-- xor [False, True, False] == True
-- xor [False, True, False, False, True] == False
-- Your solution must be implemented using a fold.
xor'' :: [Bool] -> Bool
xor'' = odd . length . filter id

xor :: [Bool] -> Bool
xor = foldr go False
  where go False True = True
        go True False = True
        go _ _        = False

-- 2.  Implement map as a fold. That is, complete the definition
-- map’ :: (a -> b) -> [a] -> [b]
-- map’ f = foldr ...
-- in such a way that map’ behaves identically to the standard
-- map function.
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x ys -> f x : ys) []

-- 3 .  (Optional) Implement foldl using foldr. That is, complete the definition
-- myFoldl :: (a -> b -> a) -> a -> [b] -> a
-- myFoldl f base xs = foldr ...
-- in such a way that myFoldl behaves identically to the standard foldl function.
-- Hint: Study how the application of foldr and foldl work out:
-- foldr f z [x1, x2, ..., xn] == x1 ‘f‘ (x2 ‘f‘ ... (xn ‘f‘ z)...)
-- foldl f z [x1, x2, ..., xn] == (...((z ‘f‘ x1) ‘f‘ x2) ‘f‘...) ‘f‘ xn
foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f z = foldr (flip f) z . reverse

-- Exercise 4 : Finding primes
-- Read about the Sieve of Sundaram.
-- Implement the algorithm using function composition. Given an integer n,
-- your function should generate all the odd prime numbers up to 2 n + 2.
-- sieveSundaram :: Integer -> [Integer]
-- sieveSundaram = ...
-- To give you some help, below is a function to compute the
-- Cartesian product of two lists. This is similar to zip, but it produces all
-- possible pairs instead of matching up the list elements. For example,
-- cartProd [1,2] [’a’,’b’] == [(1,’a’),(1,’b’),(2,’a’),(2,’b’)]
-- It’s written using a list comprehension, which we haven’t talked about
-- in class (but feel free to research them).
cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map ((+1) . (*2)) $ filter sieve [1..n]
  where sieve = not . (`elem` filter (<=n) [i + j + 2*i*j | j <- [1..n], i <- [1..j]])
