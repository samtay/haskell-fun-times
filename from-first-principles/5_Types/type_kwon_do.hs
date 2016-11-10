module TypeKwonDo where

--1.
f :: Int -> String
f = undefined

g :: String -> Char
g = undefined

h :: Int -> Char
h = g . f -- answer

--2.
data A
data B
data C

q :: A -> B
q = undefined

w :: B -> C
w = undefined

e :: A -> C
e = w . q

--4.
munge :: (x -> y) -> (y -> (w, z)) -> x -> w
munge xToy yTowz a = fst $ yTowz (xToy a)
