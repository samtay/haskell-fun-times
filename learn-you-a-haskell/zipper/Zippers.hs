module Zippers where

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)
data Direction = L | R deriving (Show)
type Directions = [Direction]

changeT :: Char -> Directions -> Tree Char -> Tree Char
changeT c (L:ds) (Node x l r) = Node x (changeT c ds l) r
changeT c (R:ds) (Node x l r) = Node x l (changeT c ds r)
changeT c [] (Node _ l r) = Node c l r

freeTree :: Tree Char
freeTree =
    Node 'P'
        (Node 'O'
            (Node 'L'
                (Node 'N' Empty Empty)
                (Node 'T' Empty Empty)
            )
            (Node 'Y'
                (Node 'S' Empty Empty)
                (Node 'A' Empty Empty)
            )
        )
        (Node 'L'
            (Node 'W'
                (Node 'C' Empty Empty)
                (Node 'R' Empty Empty)
            )
            (Node 'A'
                (Node 'A' Empty Empty)
                (Node 'C' Empty Empty)
            )
        )

elemAt :: Directions -> Tree a -> a
elemAt (L:ds) (Node _ l _) = elemAt ds l
elemAt (R:ds) (Node _ _ r) = elemAt ds r
elemAt [] (Node x _ _) = x

-- This is cool, but inefficient since we need to walk the length of the tree every time

type Breadcrumbs' = [Direction]

goLeft' :: (Tree a, Breadcrumbs') -> (Tree a, Breadcrumbs')
goLeft' (Node _ l _, bs) = (l, L:bs)

goRight' :: (Tree a, Breadcrumbs') -> (Tree a, Breadcrumbs')
goRight' (Node _ _ r, bs) = (r, R:bs)

(-:) :: a -> (a -> b) -> b
(-:) x f = f x

wNode :: (Tree Char, Breadcrumbs')
wNode = (freeTree, []) -: goRight' -: goLeft'

-- This is cool, but once we goLeft or goRight, we lose the rest of the parent
-- tree and cant "go back up" from that return value. So let's keep all the data that we need

data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving (Show)
type Breadcrumbs a = [Crumb a]

goLeft :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
goLeft (Node x l r, bs) = (l, (LeftCrumb x r):bs)

goRight :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
goRight (Node x l r, bs) = (r, (RightCrumb x l):bs)

goUp :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
goUp (t, LeftCrumb x r:bs) = (Node x t r, bs)
goUp (t, RightCrumb x l:bs) = (Node x l t, bs)

-- This type of structure is known as Zipper
type Zipper a = (Tree a, Breadcrumbs a)

modify :: (a -> a) -> Zipper a -> Zipper a
modify f (Node x l r, bs) = (Node (f x) l r, bs)
modify f (Empty, bs) = (Empty, bs)

attach :: Tree a -> Zipper a -> Zipper a
attach t (_, bs) = (t, bs)

topMost :: Zipper a -> Zipper a
topMost (t, []) = (t, [])
topMost z = topMost . goUp $ z

-- Let's make [] into a Zipper

type ListZipper a = ([a], [a])

goForward :: ListZipper a -> ListZipper a
goForward (x:xs, bs) = (xs, x:bs)

goBack :: ListZipper a -> ListZipper a
goBack (xs, b:bs) = (b:xs, bs)

-- thats a bit easier. we're just balancing a stack [abcd] [] --> [bcd] [a] --> ...
