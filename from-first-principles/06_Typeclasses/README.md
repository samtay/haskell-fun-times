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

### 6.14 Chapter Exercises

#### Does it typecheck?
```haskell
data Person = Person Bool
printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)
```
**No**, `Person` has no instance of the `Show` typeclass.

```haskell
data Mood = Blah | Woot deriving Show
settleDown x = if x == Woot
                 then Blah
                 else x
```
**No**, `Mood` has no instance of the `Eq` typeclass.

```haskell
type Subject = String
type Verb = String
type Object = String
data Sentence =
Sentence Subject Verb Object
deriving (Eq, Show)
s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"
```
**Yes**, nothing wrong with this, but `s1` is a partially applied data constructor of type `Object -> Sentence`.

#### Typechecking datatypes
Given
```haskell
data Rocks = Rocks String deriving (Eq, Show)

data Yeah = Yeah Bool deriving (Eq, Show)

data Papu = Papu Rocks Yeah deriving (Eq, Show)
```
which of the following typechecks?

```haskell
phew = Papu "chases" True
-- No, this is missing data construtors for Rocks and Yeah

truth = Papu (Rocks "chomskydoz") (Yeah True)
-- Yes this is fine

equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'
-- This is fine but unnecessary since it is equivalent to ==

comparePapus :: Papu -> Papu -> Bool
comparePapus p p' = p > p'
-- No, there is no instance of the Ord typeclass
```
