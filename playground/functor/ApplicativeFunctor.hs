{-- Applicative definition:

class (Functor f) => Applicative f where  
    pure :: a -> f a  
    (<*>) :: f (a -> b) -> f a -> f b

ex: pure (+3) <*> Just 9       == Just 12

Laws:
1. pure f <*> x         = fmap f x
2. pure id <*> v        = v
3. pure f <*> pure x    = pure (f x)
4. u <*> pure y         = pure ($ y) <*> u

--}

main = do
  a <- (++) <$> getLine <*> getLine
  putStrLn $ "The two lines concatted: " ++ a

{--

liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c  
liftA2 f a b = f <$> a <*> b 

takes a binary function and promotes it to a function that operates on two functors

ex: liftA2 (:) (Just 3) (Just [4])      == Just [3,4]

--}
