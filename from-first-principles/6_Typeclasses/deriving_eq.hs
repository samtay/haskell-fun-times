module DerivingEq where

data TwoIntegers =
  Two Integer Integer

instance Eq TwoIntegers where
  (Two j k) == (Two l m) = j == l && k == m

data StringOrInt =
    TisAnInt Int
  | TisAString String

instance Eq StringOrInt where
  TisAnInt x == TisAnInt y = x == y
  TisAString s == TisAString t = s == t
  _ == _ = False


data Pair a =
  Pair a a

instance (Eq a) => Eq (Pair a) where
  Pair j k == Pair l m = j == l && k == m

data EitherOr a b =
  Hello a
  | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  Hello x == Hello y = x == y
  Goodbye x == Goodbye y = x == y
  _ == _ = False
