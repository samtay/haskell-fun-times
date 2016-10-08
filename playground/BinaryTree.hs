module BinaryTree
( Tree(..)
, singleton
, treeInsert
, treeElem ) where

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = Node x EmptyTree EmptyTree
treeInsert x (Node y a b) = case (x `compare` y) of
    LT -> Node y (treeInsert x a) b
    GT -> Node y a (treeInsert x b)
    EQ -> Node y a b

{-- or guards
treeInsert x (Node y a b)
    | x < y  = Node y (treeInsert x a) b
    | x > y  = Node y a (treeInsert x b)
    | x == y = Node y a b
--}
treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node y a b)
  | x == y    = True
  | otherwise = treeElem x (if x < y then a else b)
