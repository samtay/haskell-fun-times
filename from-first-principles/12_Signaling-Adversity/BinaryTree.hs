module BinaryTree where

data BinaryTree a =
      Leaf
    | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Ord, Show)

-- 1. Write unfold for BinaryTree.
unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold f x = case f x of
               Just (y,b,z) -> Node (unfold f y) b (unfold f z)
               Nothing      -> Leaf

-- 2. Make a tree builder.  Using the unfold function you’ve just made for BinaryTree, write the following function:
treeBuild :: Integer -> BinaryTree Integer
treeBuild = unfold treeBuild'
  where treeBuild' n
          | n <= 0    = Nothing
          | otherwise = Just (n-1,n,n-1)

-- Hmm... these work, but give mirrored results:
-- my funcs:
-- λ> treeBuild 3
-- Node (Node (Node Leaf 0 Leaf) 1 (Node Leaf 0 Leaf))
--      2
--      (Node (Node Leaf 0 Leaf) 1 (Node Leaf 0 Leaf))
--
-- book funcs:
-- λ> treeBuild 3
-- Node (Node (Node Leaf 2 Leaf) 1 (Node Leaf 2 Leaf))
--      0
--      (Node (Node Leaf 2 Leaf) 1 (Node Leaf 2 Leaf))

-- Here we go, this one works as intended. Keeping the other one for reference.
treeBuild' :: Integer -> BinaryTree Integer
treeBuild' n = unfold treeBuild'' 0
  where treeBuild'' i
          | i >= n    = Nothing
          | otherwise = Just (i+1,i,i+1)

