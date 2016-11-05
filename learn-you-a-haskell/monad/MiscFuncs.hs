module MiscFuncs where
import Control.Monad
import Control.Monad.Trans.Writer
import Knight
{-- liftM === fmap as long as laws are satisfied

liftM :: (Monad m)  => (a -> b) -> m a -> m b
fmap :: (Functor f) => (a -> b) -> f a -> f b

liftM f m = m >>= (\x -> return f x)
liftM f m = do
    x <- m
    return (f x)

--- ap === <*> as long as laws are satisfied

ap  :: (Monad m)       => m (a -> b) -> m a -> m b
<*> :: (Applicative f) => f (a -> b) -> f a -> f b

ap mf m = do
    f <- mf
    x <- m
    return (f x)

--- join flattens monadic values
join :: (Monad m) => m (m a) -> m a
join mm = do
    m <- mm
    m

--- WHOA ---
m >>= f  ===  join (fmap f m)

This makes sense.. f :: a -> m a, so mapping results in nested monadic values... join just unnests them.


filterM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
--}
keepSmall :: Int -> Writer [String] Bool
keepSmall x
    | x < 4 = do
        tell ["Keeping " ++ show x]
        return True
    | otherwise = do
        tell ["Tossing " ++ show x]
        return False

powerset :: [a] -> [[a]]
powerset = filterM (\x -> [True, False])
{--
foldl ::              (a -> b -> a)   -> a -> [b] -> a
foldM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
--}
binSmalls :: Int -> Int -> Maybe Int
binSmalls acc x
    | x > 9     = Nothing
    | otherwise = Just (acc + x)
-- >>> foldM binSmalls 0 [1,2,3]
-- Just 6
-- >>> foldM binSmalls 0 [1,2,3,10,4]
-- Nothing

-- A less error prone RPN calculator
readMaybe :: (Read a) => String -> Maybe a
readMaybe st = case reads st of
    [(x,"")] -> Just x
    _        -> Nothing

foldingFunction :: [Double] -> String -> Maybe [Double]
foldingFunction (x:y:ys) "*" = return ((x*y):ys)
foldingFunction (x:y:ys) "+" = return ((x+y):ys)
foldingFunction (x:y:ys) "-" = return ((y-x):ys)
foldingFunction xs numStr = liftM (:xs) (readMaybe numStr)

solveRPN :: String -> Maybe Double
solveRPN st = do
    [result] <- foldM foldingFunction [] (words st)
    -- [result] utilizes Maybe monad pattern matching resulting in Nothing
    return result

-- Improve knight function (in3 => inX)
inX :: Int -> KnightPos -> [KnightPos]
inX x start = return start >>= foldr (<=<) return (replicate x possibleMoves)
-- ^ dope pattern ! (<=<) is monadic func composition, initial starting function is return (like id)

canReachIn :: Int -> KnightPos -> KnightPos -> Bool
canReachIn x start end = end `elem` inX x start
