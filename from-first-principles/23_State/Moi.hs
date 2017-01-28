{-# LANGUAGE InstanceSigs #-}
module Moi where

newtype Moi s a =
  Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) = Moi $
    \s -> let (a, sg) = g s
           in (f a, sg)

instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi $ \s -> (a, s)
  (<*>) :: Moi s (a -> b)
        -> Moi s a
        -> Moi s b
  (Moi f) <*> (Moi g) = Moi $
    \s -> let (fab, sf) = f s
              (a, sg)   = g sf
           in (fab a, sg)

instance Monad (Moi s) where
  return = pure
  (>>=) :: Moi s a
        -> (a -> Moi s b)
        -> Moi s b
  (Moi f) >>= g = Moi $
    \s -> let (a, sf) = f s
           in runMoi (g a) sf

-- 1
get :: Moi s s
get = Moi $ \s -> (s,s)

-- 2
put :: s -> Moi s ()
put s = Moi $ const ((), s)

-- 3
exec :: Moi s a -> s -> s
exec (Moi sa) s = snd $ sa s

-- 4
eval :: Moi s a -> s -> a
eval (Moi sa) s = fst $ sa s

-- 5
modify :: (s -> s) -> Moi s ()
modify f = Moi $ \s -> ((), f s)
