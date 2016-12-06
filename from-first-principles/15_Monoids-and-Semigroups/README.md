# Monoids and semigroups
Of course one of the main reasons I've come to love Haskell is for the community's propensity for recognizing abstract patterns in code which have well-defined, lawful representations in mathematics.

### 15.2 What we talk about when we talk about algebras
In mathematics, algebra generally refers to a set with some operations defined on it. In Haskell, an algebra refers to a type (set of values) along with a typeclass (operations). The instance defines how each of a typeclass's operations will perform on a given type.

### 15.3 Monoid
A **monoid** is a set with a binary associative operation with an identity.

### 15.4 How Monoid is defined in Haskell
Typeclasses give us a way to recognize, organize, and use common functionalities and patterns across types that differ in some ways but also have things in common. We think of types as *having* an instance of a typeclass.  When we represent abstract operations that can be reused across a set of types, we usually represent them as a typeclass.  The typeclass `Monoid` is defined:
```haskell
class Monoid m where
  mempty :: m
  mappend :: m -> m -> m
  mconcat :: [m] -> m
  mconcat = foldr mappend mempty
```
An example instance is `List`:
```haskell
instance Monoid [a] where
  mempty = []
  mappend = (++)

Prelude> [1,2] `mappend` [3,4]
[1,2,3,4]
```

### 15.6 Why Integer doesn't have a Monoid
Remember that a type's instance of a typeclass is **unique** for that type. We don't want to give `Integer` a direct instance of `Monoid` because there are multiple possible interpretations, e.g. addition and multiplaction. This is a classic use case for newtypes, so the `Monoid` library exports `Sum` and `Product` newtypes that defined `instance Num a => Monoid (NewType a)`.

#### Why newtype? (an aside)
Of course, one obvious difference between
```haskell
data Server = Server String
newtype Server = Server String
```
from a *developer* perspective is that `newtype` **constrains** the datatype to a single unary data constructor. Furthermore, `newtype` guarantees no additional runtime overhead from wrapping the original type, since the runtime representation is identical to the original type.

In summary, some uses cases are:
1. Signaling intent that you are simply wrapping a type, and that the type will not eventually grow into a more complicated sum or product type.
2. Improve type safety by avoiding mixing up many values of the same representation.
3. Add different typeclass instances to a type that is otherwise unchanged representationally.

### 15.8 Laws
These algebras that we define in Haskell are defined not just by the functions we implement, but by laws that come from mathematics. Laws are what really define these algebras and what makes them so useful to us as repeatable patterns. Here are the laws for `Monoid`:
```haskell
-- left identity
mappend mempty x = x

-- right identity
mappend x mempty = x

-- associativity
mappend x (mappend y z) = mappend (mappend x y) z
mconcat = foldr mappend mempty
```

### 15.9 Different instance, same representation
#### Bool
Similar to numbers having more than one possible `Monoid` instance, so does `Bool`. So we have newtypes `All` for which `mappend` returns `True` iff all inputs are `True` and `Any` for which `mappend` returns `True` iff at least one input is `True`.

#### Maybe
`Maybe` actually has three possible monoid interpretations. `Nothing` is the obvious choice for `mempty` in all situations:
```haskell
First prioritizes the leftmost non-Nothing value
Prelude> First (Just 1) `mappend` First (Just 2)
First {getFirst = Just 1}

Second prioritizes the rightmost non-Nothing value
Prelude> Last (Just 1) `mappend` Last (Just 2)
Last {getLast = Just 2}
``` 
The third interpretation uses the general pattern of reusing Monoid instances in the components of larger types.

### 15.10 Reusing algebras by asking for algebras
#### Exercise: Write the Maybe Monoid instance renamed to Optional:
```haskell
data Optional a = Nada | Only a
  deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend Nada (Only x)     = Only x
  mappend (Only x) Nada     = Only x
  mappend (Only x) (Only y) = Only (x `mappend` y)
```

#### The problem of orphan instances
Recall (again) that typeclasses have **unique** pairings of the class & instance for a given type. Sometimes we end up with multiple instances for a single type when orphan instances are written -- **this should be avoided at all costs**.

An *orphan instance* is when an instance is defined for a datatype and typeclass, but not in the same module as the declaration of either the typeclass or datatype. (GHC will warn about this.) If you don't "own" the typeclass or datatype, newtype it!

If you
1. Defined the type but not the typeclass: put the instance in the same module as the type so that the type cannot be imported without its instances.
2. Defined the typeclass but not the type: put the instance in the same module as the typeclass definition so that the typeclass cannot be imported without its instances.
3. Defined neither the type nor typeclass: define your own newtype to wrap the original type - now it belongs to you.

### 15.12 Better living through QuickCheck
Proving laws can be tedious -- QuickCheck is a good compromise to get a sense of whether or not the laws are *likely* obeyed.
```haskell
-- You can bind infix names for func arguments! Sick.
-- Test whether the the first function argument treats the last three value arguments associatively.
asc :: Eq a => (a -> a -> a) -> a -> a -> a -> Bool
asc (<>) a b c =
  a <> (b <> c) == (a <> b) <> c

-- With QuickCheck:
import Data.Monoid
import Test.QuickCheck

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

quickCheck (monoidAssoc :: String -> String -> String -> Bool) -- explicit types
-- etc
```

See examples in this directory for a [bad monoid](./BadMonoid.hs) and a [good monoid](./AnotherMaybeMonoid.hs), both with QuickCheck tests.

### 15.13 Semigroup
A semigroup is just a more generalized monoid -- specifically it is a set with an associative binary operation, but it need not have an identity:
```haskell
class Semigroup a where
  (<>) :: a -> a -> a

-- Law of associativity
-- (a <> b) <> c = a <> (b <> c)
```

#### NonEmpty
One really useful datatype that can’t have a `Monoid` instance but does have a `Semigroup` instance is the `NonEmpty` list type. It is a list datatype that can never be an empty list:
```haskell
data NonEmpty a = a :| [a]
  deriving (Eq, Ord, Show)

-- It's just like a list but the first element, which is required,
-- remains visible as the first argument to to the infix data
-- constructor :|

>>> let xs = 1 :| [2, 3]
>>> let ys = 4 :| [5, 6]
>>> xs <> ys
1 :| [2,3,4,5,6]
>>> N.head xs
1
```
Obviously, by construction, there is no canonical empty value that can be used as an identity, making this a natural fit for Semigroup. 
