# Foldable
The `Foldable` typeclass generalizes catamorphisms past the list type. The folding function is always dependent on some Monoid instance.

### 20.2 The Foldable class
Notice the `MINIMAL` annotation that allows us to minimally define an instance with either `foldMap` **or** `fold`:
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
