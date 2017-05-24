module Fibonacci where

fib :: Integer -> Integer
fib n
  | n < 0 && even n = negate $ fib (-n)
  | n < 0 && odd n  = fib (-n)
  | otherwise       = let (M _ m _ _) = (M 1 1 1 0)^n in m

-- 2 x 2 matrix
data M = M Integer Integer
           Integer Integer
         deriving (Eq, Show)

instance Num M where
  (M a b c d) * (M w x y z) = M (a*w + b*y) (a*x + b*z) (c*w + d*y) (c*x + d*z)
  (M a b c d) + (M w x y z) = M (a+w) (b+x) (c+y) (d+z)
  abs (M a b c d) = M (abs a) (abs b) (abs c) (abs d)
  signum (M a b c d)
    | a + b + c + d < 0 = (-1)
    | a + b + c + d > 0 = 1
    | otherwise         = 0
  fromInteger n = M n 0 0 n
  negate (M a b c d) = M (negate a) (negate b) (negate c) (negate d)
