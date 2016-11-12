module FunckyStuff where


-- 1. The following function returns the tens digit of an integral argument.
tensDigit' :: Integral a => a -> a
tensDigit' x = d
  where xLast = x `div` 10
        d = xLast `mod` 10
-- a) First, rewrite it using divMod.
tensDigit :: Integral a => a -> a
tensDigit = snd . (`divMod`10) . fst . (`divMod`10)
-- b) Does the divMod version have the same type as the original version?
-- Yes of course

-- c) Next, let’s change it so that we’re getting the hundreds digit instead.
hunD :: Integral a => a -> a
hunD = snd . (`divMod`10) . fst . (`divMod`100)


-- 2. Implement the function of the type a -> a -> Bool -> a once each using a case expression and once with a guard.
foldBool :: a -> a -> Bool -> a
foldBool x y check = case check of
                          True -> x
                          False -> y

foldBool' x y check
  | check     = x
  | otherwise = y


-- 3. Fill in the definition. Note that the first argument to our function is also a function which can be applied to values. Your second argument is a tuple, which can be used for pattern matching:
g :: (a -> b) -> (a, c) -> (b, c)
g f (x, y) = (f x, y)


-- 5. Next, write a pointfree version of roundTrip:
roundTrip' :: (Show a, Read a) => a -> a
roundTrip' a = read (show a)
-- point free:
roundTrip :: (Show a, Read a) => a -> a
roundTrip = read . show


-- 6. Your task now is to change the type of roundTrip to (Show a, Read b) => a -> b. How might we tell GHC which instance of Read to dispatch against the String now? Make the expression print (roundTrip 4) work. You will only need the has the type syntax of :: and parentheses for scoping.
roundTripImproved :: (Show a, Read b) => a -> b
roundTripImproved = (read . show)
-- Not sure what the author wants here.. I think definition stays the same, but we modify
-- print (roundTrip 4)
-- to
-- print (roundTripImproved 4 :: Int)
