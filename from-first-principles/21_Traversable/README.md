# 21 Traversable
Traversable was introduced in the same paper as Applicative, and while it's been in the `base` library for a while, it was only recently added to the Prelude in GHC 7.10. It depends on Applicative, Functor, and is superclassed by Foldable.

Traversable allows the transformation of elements inside a structure like a Functor, producing Applicative results along the way, and lifts those potentially multiple instances of Applicative structure outside of the Traversable structure.

### 21.2 The Traversable typeclass definition
```haskell
class (Functor t, Foldable t) => Traversable t where
  {-# MINIMAL traverse | sequenceA #-}

  -- | Map each element of a structure to an action,
  -- evaluate these actions from left to right, and
  -- collect the results. For a version that ignores
  -- the results see 'Data.Foldable.traverse_'.
  traverse :: Applicative f =>
              (a -> f b)
           -> t a
           -> f (t b)
  traverse f = sequenceA . fmap f

  -- | Evaluate each action in the structure from
  -- left to right, and collect the results.
  -- For a version that ignores the results see
  -- 'Data.Foldable.sequenceA_'.
  sequenceA :: Applicative f => t (f a) -> f (t a)
  sequenceA = traverse id
```

### 21.3 `sequenceA`
We can see from the type signature that `sequenceA` basically flips the contexts of Applicative and Traversable - there is no function application to the inner type `a` value.
```haskell
λ> fmap Just [1, 2, 3]
[Just 1, Just 2, Just 3]

λ> sequenceA [Just 1, Just 2, Just 3]
Just [1, 2, 3]

-- Nothing clobbers the Maybe applicative
λ> sequenceA [Just 1, Just 2, Nothing]
Nothing

λ> fmap sum $ sequenceA [Just 1, Just 2, Just 3]
Just 6

λ> fmap sum $ sequenceA [Just 1, Just 2, Nothing]
Nothing
```

### 21.4 `traverse`
Let's compare `traverse` with a few familiar functions:
```haskell
fmap     :: (a -> b) -> f a -> f b
(=<<)    :: (a -> m b) -> m a -> m b
traverse :: (a -> f b) -> t a -> f (t b)
   where (Applicative f, Traversable t)
```
There is still a mapping of a function over embedded values, like `fmap`, but similar to `flip bind`, that function is itself generating more structure (but that structure can be of a different type than the structure we're lifting over to apply the function). Then, the structures are flipped just like `sequenceA`. This is easily seen from the definition:
```haskell
traverse f = sequenceA . fmap f

λ> fmap Just [1, 2, 3]
[Just 1,Just 2,Just 3]

λ> sequenceA $ fmap Just [1,2,3]
Just [1,2,3]

λ> sequenceA . fmap Just $ [1, 2, 3]
Just [1,2,3]

λ> traverse Just [1, 2, 3]
Just [1,2,3]
```

#### mapM is just traverse
Prior to GHC 7.10, `mapM` had type:
```haskell
mapM :: Monad m => (a -> m b) -> [a] -> m [b]

-- constrasted with

traverse :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
```

So we see that `traverse` abstracts `[]` to any `Traversable t` and `Monad m` to `Applicative f`.
This way, we can use this pattern across more types.
One specific scenario where this is handy is when squeezing out performance, using the Vector Traversable instance instead of a list.

In the same way, `sequenceA` is a more useful generalized version of the `sequence` prior to 7.10:
```haskell
sequence  :: Monad m                        => [m a]   -> m [a]
sequenceA :: (Applicative f, Traversable t) => t (f a) -> f (t a)
```

### 21.5 So, what's traversable for?
Vaguely, anytime you need to flip two type constructors around, or map something and then flip them around, you can probably use Traversable.
Here are some examples of getting types the way we want them:
```haskell
f        :: a -> Maybe b
xs       :: [a]
map f xs :: [Maybe b]

-- if we want Maybe [b]

sequenceA $ map f xs :: Maybe [b]
traverse f xs        :: Maybe [b]
```

### 21.6 Morce code revisited
Back in the [morse code example](../14_Testing/morse/src/Morse.hs).
