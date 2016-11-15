module CustomFolds where

-- 2. myAny returns True if a -> Bool applied to any of the values in the list returns True.
myAny :: (a -> Bool) -> [a] -> Bool
myAny p = foldr (\x acc -> if p x then True else acc) False

-- 4. Implement myReverse, don’t worry about trying to make it lazy.
myReverse :: [a] -> [a]
myReverse = foldr (\x acc -> acc ++ [x]) []

-- 5. Write myMap in terms of foldr. It should have the same behavior as the built-in map.
myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\x acc -> (f x):acc) []

-- 10. myMaximumBy takes a comparison function and a list and returns the greatest element of the list based on the last value that the comparison returned GT for.
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy m xs = foldr (\x acc -> case m x acc of
                      GT -> x
                      EQ -> acc
                      LT -> acc
                      ) (head xs) xs
{--
myMaximumBy _ []     = error "u use dis wrong"
myMaximumBy m (x:xs) = foldr (\y acc -> case m y acc of
                         GT -> y
                         EQ -> acc
                         LT -> acc
                         ) x xs
--}

{-- Not clear what the author wants for 10. The return values:

Prelude> myMaximumBy (\_ _ -> GT) [1..10]
1
Prelude> myMaximumBy (\_ _ -> LT) [1..10]
10
Prelude> myMaximumBy compare [1..10]
10

Dont particularly make any sense... messed with a few variations of this and didn't get those results.
--}
