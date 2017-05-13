module DList where

import Criterion.Main

newtype DList a = DL { unDL :: [a] -> [a] }

-- 1
empty :: DList a
empty = DL id
{-# INLINE empty #-}

-- 2
singleton :: a -> DList a
singleton x = DL (x:)
{-# INLINE singleton #-}

-- 3
toList :: DList a -> [a]
toList xs = unDL xs []
{-# INLINE toList #-}

-- 4 -- Prepend a single element to a dlist.
infixr `cons`
cons :: a -> DList a -> DList a
cons x xs = DL ((x:) . unDL xs)
{-# INLINE cons #-}

-- 5 -- Append a single element to a dlist.
infixl `snoc`
snoc :: DList a -> a -> DList a
snoc xs x = DL (unDL xs . (x:))
{-# INLINE snoc #-}

-- 6 -- Append dlists.
append :: DList a -> DList a -> DList a
append xs ys = DL (unDL xs . unDL ys)
{-# INLINE append #-}

-- TEST: running main, the DList variant should be twice as fast as the normal list
schlemiel :: Int -> [Int]
schlemiel i = go i []
  where go 0 xs = xs
        go n xs = go (n-1) ([n] ++ xs)

constructDlist :: Int -> [Int]
constructDlist i = toList $ go i empty
  where go 0 xs = xs
        go n xs = go (n-1) (singleton n `append` xs)

main :: IO ()
main = defaultMain
  [ bench "concat list" $ whnf schlemiel 123456
  , bench "concat dlist" $ whnf constructDlist 123456
  ]
