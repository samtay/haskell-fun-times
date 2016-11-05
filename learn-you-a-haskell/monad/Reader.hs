module ReaderStuff where
-- Remember ( (->) r ) is both a functor and applicative
-- fmap (*5) (+3)  ===  (*5) <$> (+3)  === (*5) . (+3)  ===  \x -> (x+3) * 5

-- >>> let f = (+) <$> (*2) <*> (+10)   ====   ( (+) <$> (*2) ) <*> (+10)
-- --- === \x -> (x*2) + (x+10)
-- >>> f 3
-- 19


{-- Functions (r ->) as Monads

instance Monad ((->) r) where
    return x = \_ -> x
    h >>= f = \w -> f (h w) w

--}

-- accomplishes the <$> <*> stuff above, relying on monad capabilities
addStuff :: Int -> Int
addStuff = do
    a <- (*2)
    b <- (+10)
    return (a+b)
