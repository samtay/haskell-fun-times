# Typeclasses

### 6.4 Eq

#### Typeclass deriving
Typeclass instances we can magically derive are Eq, Ord, Enum, Bounded, Read, and Show, though there are some constraints on deriving some of these.

#### Partial functions
A *partial function* is one that doesn't handle all the possible cases of input, such as
```
addOne :: Int -> Int
addOne 0 = 1
addOne 1 = 2
```
We can catch these **at compile time** if we enable all warnings via GHC flag `Wall`.

#### Exercises: Write the `Eq` instance for each datatype
```haskell
data TwoIntegers =
  Two Integer Integer

instance Eq TwoIntegers where
  (Two j k) == (Two l m) = j == l && k == m
  _ == _ = False

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
  _ == _ = False

data EitherOr a b =
  Hello a
  | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  Hello x == Hello y = x == y
  Goodbye x == Goodbye y = x == y
  _ == _ = False
```
