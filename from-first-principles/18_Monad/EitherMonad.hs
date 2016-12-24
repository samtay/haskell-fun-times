module EitherMonad where

import Control.Applicative (liftA)

data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap = liftA

instance Applicative (Sum a) where
  pure      = return
  (<*>) f x = x >>= (\x' -> f >>= (\f' -> return $ f' x'))

instance Monad (Sum a) where
  return         = Second
  First x >>= _  = First x
  Second y >>= f = f y
