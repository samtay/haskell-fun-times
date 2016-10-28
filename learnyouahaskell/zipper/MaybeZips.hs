module MaybeZips where

import Zippers (Tree(..), Crumb(..), Breadcrumbs, Zipper, freeTree)

-- Implementing error handling via maybe monad

goLeft :: Zipper a -> Maybe (Zipper a)
goLeft (Node x l r, bs) = Just (l, LeftCrumb x r:bs)
goLeft (Empty, _) = Nothing

goRight :: Zipper a -> Maybe (Zipper a)
goRight (Node x l r, bs) = Just (r, RightCrumb x l:bs)
goRight (Empty, _) = Nothing

goUp :: Zipper a -> Maybe (Zipper a)
goUp (t, LeftCrumb x r:bs) = Just (Node x t r, bs)
goUp (t, RightCrumb x l:bs) = Just (Node x l t, bs)
goUp (_, []) = Nothing

-- Notice that before we had f :: a -> a, but now we have f :: a -> Maybe a
-- Look familiar?
--       1
--    _     3
--         _ _
coolTree :: Tree Int
coolTree = Node 1 Empty (Node 3 Empty Empty)

coolerTree :: Maybe (Zipper Int)
coolerTree = return (coolTree, []) >>= goRight
failureTree :: Maybe (Zipper Int)
failureTree = return (coolTree, []) >>= goRight >>= goRight >>= goRight
