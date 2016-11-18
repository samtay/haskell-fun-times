{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module LogicGoats where

class TooMany a where
  tooMany :: a -> Bool
  
instance TooMany Int where
  tooMany n = n > 42

newtype Goats = Goats Int deriving (Eq, Show, TooMany)

instance TooMany (Int, String) where
  tooMany (n, s) = n > 40

instance TooMany (Int, Int) where
  tooMany (n, m) = n + m > 40

instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany (n, m) = tooMany n || tooMany m
