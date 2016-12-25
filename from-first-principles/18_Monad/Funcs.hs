module Funcs where

import Control.Monad

-- 1
j :: Monad m => m (m a) -> m a
j = (>>= id)
{-
This was a little confusing at first. First off, it makes sense from an equational
reasoning perspective, letting 'a' == 'm b':

λ> :t id
id :: a -> a

λ> :t (>>=)
(>>=) :: Monad m => m a -> (a -> m b) -> m b

λ> :t (>>= id)
(>>= id) :: Monad m => m (m b) -> m b

Furthermore, remember when we define >>= for particular monad instances,
and we are for the most part expecting values "not in structure" mapped
to values "in structure", that is: `a -> m b`.

Since id has type `a -> a`, or specialized in structure: `m a -> m a`, we can see that
passing it into a function expecting `a -> m a`, or equivalently `m a -> m (m a)`,
will end up "reducing" the structure.
-}

-- 2
l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

-- 3
l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f x y = f <$> x <*> y

-- 4
a :: Monad m => m a -> m (a -> b) -> m b
a = flip (<*>)

-- 5
meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh xs f = let mcons = liftM2 (:)
            in foldr (mcons . f) (return []) xs

-- 6 Hint: reuse “meh”
flipType :: (Monad m) => [m a] -> m [a]
flipType xs = meh xs id


