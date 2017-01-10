# Foldable
The `Foldable` typeclass generalizes catamorphisms past the list type. The folding function is always dependent on some Monoid instance.

### 20.2 The Foldable class
Notice the `MINIMAL` annotation that allows us to minimally define an instance with either `foldMap` **or** `foldr`:
```haskell
class Foldable (t :: * -> *) where
  {-# MINIMAL foldMap | foldr #-}
  fold :: Monoid m => t m -> m
  foldMap :: Monoid m => (a -> m) -> t a -> m
  foldr :: (a -> b -> b) -> b -> t a -> b
  foldr' :: (a -> b -> b) -> b -> t a -> b
  foldl :: (b -> a -> b) -> b -> t a -> b
  foldl' :: (b -> a -> b) -> b -> t a -> b
  foldr1 :: (a -> a -> a) -> t a -> a
  foldl1 :: (a -> a -> a) -> t a -> a
  toList :: t a -> [a]
  null :: t a -> Bool
  length :: t a -> Int
  elem :: Eq a => a -> t a -> Bool
  maximum :: Ord a => t a -> a
  minimum :: Ord a => t a -> a
  sum :: Num a => t a -> a
  product :: Num a => t a -> a
```

### 20.3 Revenge of the monoids
Folding necessarily implies a binary associative operation that has an identity value, which is why the `Monoid` constraint is explicit in `fold` and `foldMap`.
```haskell
fold :: Monoid m => t m -> m
foldMap :: Monoid m => (a -> m) -> t a -> m
```
Notice we don't need an explicit constraint in the other class functions, since we specify the binary operation and identities in the usage:
```haskell
foldr :: (a -> b -> b) -> b -> t a -> b
```

As an example, here's how we'd fold over:
```haskell
-- summation
Prelude> fold [1, 2, 3, 4, 5 :: Sum Integer]
Sum {getSum = 15}
Prelude> foldMap Sum [1, 2, 3, 4]
Sum {getSum = 10}

-- boolean conjunction
Prelude> foldMap All [True, False, True]
All {getAll = False}

-- boolean disjunction
Prelude> foldMap Any [(3 == 4), (9 > 5)]
Any {getAny = True}
```
Notice `foldMap` is nice for specifying a data constructor as the function creating a monoid, but it can obviously do more:
```haskell
λ> foldMap (Sum . (*2)) [1, 2, 3, 4, 5]
Sum {getSum = 30}
```

### 20.4 Demonstrating Foldable instances
#### Identity
```haskell
data Identity a = Identity a
instance Foldable Identity where
  foldr f z (Identity x) = f x z
  foldl f z (Identity x) = f z x -- not necessary to specify
  foldMap f (Identity x) = f x -- not necessary to specify
```
Usually catamorphisms reduce a bunch of values down to a summary value, but in this case the point is less to *reduce* and more to *consume* or *use* the value.
```haskell
λ> foldl (*) 5 (Identity 5)
25
```

#### Maybe
```haskell
data Optional a = Nada | Yep a
instance Foldable Optional where
  foldr _ z Nada    = z
  foldr f z (Yep x) = f x z
  -- or
  foldMap _ Nada    = mempty
  foldMap f (Yep a) = f a
```

### 20.5 Some basic derived operations
```haskell
-- | List of elements of a structure, from left to right.
toList :: t a -> [a]

λ> toList (Just 1)
[1]

-- note that the foldable instance used for tuple (a,b) is defined for ((,) a)
-- as we have seen before
λ> toList (1, 2)
[2]

-- | Test whether the structure is empty.
-- The default implementation is
-- optimized for structures that are similar
-- to cons-lists, because there
-- is no general way to do better.
null :: t a -> Bool

λ> null (Left 3)
True
λ> null []
True

-- | Returns the size/length of a finite
-- structure as an 'Int'.
The default
-- implementation is optimized for structures
-- that are similar to cons-lists, because there
-- is no general way to do better.
length :: t a -> Int

λ> length (1, 2)
1
λ> length [(1, 2), (3, 4), (5, 6)]
3

-- | Does the element occur in the structure?
elem :: Eq a => a -> t a -> Bool

λ> elem 1 (Just 3)
False
λ> elem True (Left True)
False
λ> elem True (Right True)
True

-- | The largest element of a non-empty structure.
maximum :: Ord a => t a -> a
-- | The least element of a non-empty structure.
minimum :: Ord a => t a -> a

λ> maximum [10, 12, 33]
33
λ> fmap maximum [Just 10, Just 12, Just 33]
[10, 12, 33]
λ> fmap maximum Just [10, 12, 33]
Just 33
λ> fmap minimum [Just 4, Just 3, Nothing]
[4,3,*** Exception: minimum: empty structure

-- | Computes the sum of numbers in a structure
sum :: (Foldable t, Num a) => t a -> a

-- | Computes the product of numbers in a structure
product :: (Foldable t, Num a) => t a -> a

λ> fmap sum [(7, 5), (3, 4)]
[5,4]
λ> fmap sum (Just [1, 2, 3, 4, 5])
Just 15
λ> fmap product (Just [])
Just 1
λ> fmap product (Right [1, 2, 3])
Right 6
```

#### Exercises: Library Functions
See [LibraryFuncs.hs](./LibraryFuncs.hs).

### 20.6 Chapter Exercises
See [Instances.hs](./Instances.hs).
