module Golf where

import Data.Monoid

-- Exercise 1 Hopscotch

-- | 'skips' maps a function 'fn' over an array of integers 1,..,n
-- where n is the length of the input 'xs'.
-- The mapping function 'fn' references an indexed version of 'xs': [(1,x_1), (2,x_2), ..., (n,x_n)]
-- and for a given integer i, filters this indexed list to only include elements with an
-- index divisible by i: [(i,x_i),(i*2, x_{i*2}),...]. Then 'fn' performs a simple map
-- to get the second element of each tuple, resulting in [x_i,x_{i*2},...]. This is precisely
-- the desired i-th element in the exercise description.
{-
-- First iteration
skips :: [a] -> [[a]]
skips xs = map fn [1..length xs]
  where
    fn n = map snd $
      filter (\(i,_) -> mod i n == 0) nxs
    nxs = zip [1..] xs
-}
{-
-- List comprehensions for less characters
skips :: [a] -> [[a]]
skips xs = map fn [1..length xs]
  where
    fn n = [ x | (i,x) <- zip [1..] xs, mod i n == 0]
-}
-- Anonymous function for less characters
skips :: [a] -> [[a]]
skips xs = map
  (\n -> [ x | (i,x) <- zip [1..] xs, mod i n == 0])
  [1..length xs]

-- Exercise 2 Local maxima

-- | Inner folding function basically cycles through list in groups of three.
-- Nothing represents the start of list. Since we traverse from left to right, that is
-- \z (x,y,ms) -> (y,z,ms), we should never encounter (Just x, Nothing, ms), which
-- is the second pattern match below. I could shave off characters by leaving the partial
-- function..
localMaxima :: [Integer] -> [Integer]
localMaxima = thrd . foldr lM' (Nothing,Nothing,[])
  where
    lM' z (Nothing, y, ms)         = (y,       Just z, ms)
    lM' z (_, Nothing, ms)         = (Nothing, Just z, ms)
    lM' z ((Just x), (Just y), ms) =
      (Just y, Just z, if x < y && y > z then y:ms else ms)

thrd :: (a,b,c) -> c
thrd (_,_,c) = c

-- Exercise 3 Histogram

newtype Map k v = Map { mkMap :: [(k,v)] } deriving (Eq,Show)
instance (Eq k, Monoid v) => Monoid (Map k v) where
  mempty = Map []
  mappend (Map xs) (Map ys) = foldr cons (Map ys) xs

instance Foldable (Map k) where
  foldMap toM (Map xs) = mconcat $ map (toM . snd) xs

cons :: (Monoid v, Eq k) => (k, v) -> Map k v -> Map k v
cons (k,v) (Map xs) = Map $ maybe
  ((k,v) : xs)
  (\t -> (k, v <> t) : filter ((/=k) . fst) xs)
  (lookup k xs)

max' :: (Foldable t, Ord x, Num x) => t x -> x
max' xs = if null xs then 0 else maximum xs

lookup' :: Eq a => a -> Map a b -> Maybe b
lookup' i (Map xs) = lookup i xs

histogram :: [Integer] -> String
histogram = fmt . foldr (\x ms -> (x,1) `cons` ms) (Map [])

fmt :: Map Integer (Sum Integer) -> String
fmt xs = unlines $ bars xs
  ++ [ replicate 10 '='
     , ['0'..'9'] ]

bars :: Map Integer (Sum Integer) -> [String]
bars gs = reverse $ map
  (\ln -> map (\i -> maybe ' ' (\l -> if (getSum l) >= ln then '*' else ' ') $ lookup' i gs) [0..9])
  [1..getSum $ max' gs]
