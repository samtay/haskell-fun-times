module TypeKwonDo where

--1.
chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f x y = (f x) == y -- answer

--2.
-- Hint: use some arithmetic operation to
-- combine values of type 'b'. Pick one.
arith :: Num b => (a -> b) -> Integer -> a -> b
arith f n x = (f x) * (fromInteger n) -- answer
