module ReinventTheWheel where

-- Maybe
data Possibly a =
    LolNope
  | Yeppers a
  deriving (Eq, Show)

instance Functor Possibly where
  fmap f LolNope     = LolNope
  fmap f (Yeppers x) = Yeppers (f x)

-- Either
data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap f (Second b) = Second (f b)
  fmap f (First a)  = First a

-- Why cant a Functor instance apply only to 'First'?
{-
  The types would not check. Instead of an f :: b -> c function resulting in
    (Sum a) b -> (Sum a) c
  This would result in something like
    (Sum b) a -> (Sum c) a
  Which is **not** consistent with the Functor definition where f is constant:
    f b -> f c
-}
