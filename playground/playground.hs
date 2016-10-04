{--
Wow, I was trying to implement this "caller" and then continued tutorial and found $
-- and I was damn close to standard implementation.... 
caller :: a -> (a -> b -> c) -> (b -> c)
caller x f = f x
--}

mapr' :: (a -> b) -> [a] -> [b]
mapr' f xs = foldr (\x acc -> (f x):acc) [] xs


elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys


sum' :: (Num a) => [a] -> a
-- sum' xs = foldl (+) 0 xs
sum' = foldl (+) 0 -- make use of currying!
-- foo a = bar b a   ===>   foo = bar b


numChains :: Int
numChains = length (filter (\xs -> length xs > 15) (map chain [1..100]))
-- "anonymous funcs" called lambdas


chain :: (Integral a) => a -> [a]
chain n
  | n == 1    = [1]
  | odd n     = n:chain (3*n + 1)
  | otherwise = n:chain (n`div`2)


flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
  where g x y = f y x

-- OR
-- flip' f x y = f y x ## OOOOO wow curried functions FTW !!!!

zipW' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipW' f _ []          = []
zipW' f [] _          = []
zipW' f (x:xs) (y:ys) = f x y : zipW' f xs ys


quicksort :: (Ord a) => [a] -> [a]
quicksort []      = []
quicksort (x:xs)  = 
  let smaller = quicksort [a | a <- xs, a <= x]
      bigger = quicksort [a | a <- xs, a > x]
  in smaller ++ [x] ++ bigger

rev' :: [a] -> [a]
rev' [] = []
rev' (x:xs) = rev' xs ++ [x]


take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
  | n <= 0     = []
take' n []     = []
take' n (x:xs) = x:take' (n-1) xs

rep' :: (Num i, Ord i) => i -> a -> [a]
rep' n x
  | n <=0 = []
  | otherwise = x:rep' (n-1) x

recFunc :: (Num b) => [b] -> b
recFunc [] = 0
recFunc x = 1

bmiTell :: (RealFloat a) => a -> a -> String  
bmiTell weight height  
  | bmi <= 18.5 = "You're underweight, you emo, you!"  
  | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
  | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"  
  | otherwise   = "You're a whale, congratulations!"  
  where bmi = weight / height ^ 2  

max' :: (Ord a) => [a] -> a
max' [] = error "no max of empty list"
max' [x] = x
max' (x:xs)
  | x > maxTail = x
  |otherwise = maxTail
  where maxTail = max' xs
