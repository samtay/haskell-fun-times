module Exercises where

-- Recursive divideBy function

dividedBy :: Integer -> Integer -> Integer
dividedBy num denom = fst $ go num denom 0
  where go n d i
          | n < d     = (i, n)
          | otherwise = go (n - d) d (i + 1)


-- Reviewing currying

cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

-- fill in the types

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"

-- Give the normal form

-- appedCatty "woohoo!"
-- = "woops mrow woohoo!"

-- frappe "1"
-- = "1 mrow haha"

-- frappe (appedCatty "2")
-- = frappe "woops mrow 2"
-- = "woops mrow 2 mrow haha"

-- appedCatty (frappe "blue")
-- = appedCatty "blue mrow haha"
-- = "woops mrow blue mrow haha"

-- cattyConny (frappe "pink")
--            (cattyConny "green" (appedCatty "blue"))
-- = cattyConny "pink mrow haha"
--              cattyConny "green" "woops mrow blue"
-- = cattyConny "pink mrow haha" "green mrow woops mrow blue"
-- = "pink mrow haha mrow green mrow woops mrow blue"

-- cattyConny (flippy "Pugs" "are") "awesome"
-- = (flippy "Pugs" "are") ++ " mrow awesome"
-- = "are mrow Pugs mrow awesome"


-- Recursion
-- 2. Write a function that recursively sums all numbers from 1 to n, n being the argument.
--    So that if n was 5, you’d add 1 + 2 + 3 + 4 + 5 to get 15.
--    The type should be (Eq a, Num a) => a -> a.

-- Changing typeclass from Eq -> Ord to handle negative numbers
sumr :: (Ord a, Num a) => a -> a
sumr n
  | n < 0     = error "only accepts posints"
  | n == 0    = 0
  | otherwise = n + sumr (n - 1)


-- McCarthy 91 function
mc91 :: (Ord a, Num a) => a -> a
mc91 n
  | n > 100 = n - 10
  | otherwise = mc91 . mc91 $ n + 11
  
