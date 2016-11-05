{--

Monads

- A natural extension of applicative functors. If we have a value with context `m a`
and a function `(a -> m b)` that takes a "normal" value `a` but returns a value
with context `m b`, how do we feed the context value `m a` into the function?

class Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  x >> y = x >>= \_ -> y
  fail :: String -> m a
  fail msg = error msg

Laws:
1. return x >>= f   = fx
2. m >>= return     = m
3. (m >>= f) >>= g  = m >>= (\x -> fx >>= g)

Equivalent Laws:
1. f <=< return    = f
2. return <=< f    = f
3. f <=< (g <=< h) = (f <=< g) <=< h

--}

-- Conceptualize bind (>>=)
applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe Nothing f  = Nothing
applyMaybe (Just x) f = f x

{--

instance Monad Maybe where
  return x = Just x
  Nothing >>= f = Nothing
  Just x >>= f = fx
  fail _ = Nothing
--}

-- Pierre falls if the sides of the pole differ by > 3 birds
type Birds = Int
type Pole = (Birds, Birds)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (l, r)
  | abs ((l + n) - r) < 4 = Just (l + n, r)
  | otherwise             = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight n (l, r)
  | abs (l - (r + n)) < 4 = Just (l, r + n)
  | otherwise             = Nothing

(-:) :: a -> (a -> b) -> b
x -: f = f x

-- (0,0) -: landRight 3 -: (>>= landLeft 1) -: (>>= landLeft 5) -- OR
-- return (0,0) >>= landRight 3 >>= landLeft 1 >>= landLeft 5
-- Just (6,3)

-- return (0,0) >>= landRight 3 >>= landLeft 1 >>= landLeft 5 >>= landLeft 1
-- Nothing

{-- do notation

do is for all monads, not just IO!

-- return (0,0) >>= landRight 3 >>= landLeft 1 >>= landLeft 5 -- OR
--}

routine :: Maybe Pole
routine = do
  start <- return (0,0)
  first <- landRight 3 start
  second <- landLeft 1 first
  landLeft 5 second

justH :: Maybe Char
justH = do
  (x:xs) <- Just "hello"
  return x

wopwop :: Maybe Char
wopwop = do
  (x:xs) <- Just "" -- failed pattern matching calls monad :: fail
  return 'x'        -- Maybe implements fail _ = Nothing


-- Lists as Monads

-- ghci> [1,2] >>= \n -> ['a','b'] >>= \ch -> return (n,ch)
--      [(1,'a'),(1,'b'),(2,'a'),(2,'b')]
-- equivalent to
listOfTuples :: [(Int, Char)]
listOfTuples = do
  n <- [1,2]
  ch <- ['a','b']
  return (n,ch)

-- whoa.. looks like list comprehension
-- turns out both do and list comprehensions are just syntactic sugar around >>=
-- listExample = [ (n,ch) | n <- [1,2], ch <- ['a','b'] ]

