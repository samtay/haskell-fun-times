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
Back in the [morse code example](../14_Testing/morse/src/Morse.hs):
```haskell
-- Previously, we fmapped from [Char] to [Maybe Char] and then sequenced to Maybe [Char]
stringToMorse :: String -> Maybe [Morse]
stringToMorse s = sequence $ fmap charToMorse s

-- With traverse, this is much cleaner:
stringToMorse :: String -> Maybe [Morse]
stringToMorse = traverse charToMorse
```

As an aside: we defined `traverse` above as `traverse f = sequenceA . fmap f`. We lift the function `f` by partially applying `fmap` and then use composition in the normal fashion. We could eta reduce this by composing *twice*:
```haskell
λ> :t \f -> sequence . fmap f
(Monad m, Traversable t) => (a -> m b) -> t a -> m (t b)
λ> :t (sequence .) . fmap
(Monad m, Traversable t) => (a -> m b) -> t a -> m (t b)
```
That double `.` looks a bit weird; it's because `fmap` takes *two* arguments, so we need to compose twice to await a second argument for `fmap` to get applied to. If only composed once, the second argument would not behave as expected:
```haskell
-- correct
(sequence .) . fmap = \f xs -> sequence (fmap f xs)

-- incorrect
sequence . fmap = \f -> sequence (fmap f)
```

To recap, `sequence` is the bit unique to the `Traversable` class, and `traverse` is just `sequence` and `fmap`, but you end up `fmap`ing before `sequence` very often.
This is just like how `join` is the bit unique to `Monad`, and `>>=` is `join` composed with `fmap`, but it turns out that's what you need most of the time.

### 21.7 Axing tedious code
Let's take a look at some code that needs refactoring:
```haskell
data Query     = Query
data SomeObj   = SomeObj
data IoOnlyObj = IoOnlyObj
data Err       = Err

-- There's a decoder function that makes
-- some object from String
decodeFn :: String -> Either Err SomeObj
decodeFn = undefined

-- There's a query, that runs against DB and
-- returns array of strings
fetchFn :: Query -> IO [String]
fetchFn = undefined

-- there's some additional "context initializer",
-- that also has IO side-effects
makeIoOnlyObj :: [SomeObj]
              -> IO [(SomeObj, IoOnlyObj)]
makeIoOnlyObj = undefined

-- the function we're examining
pipelineFn :: Query
           -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn query = do
  a <- fetchFn query
  case sequence (map decodeFn a) of
    (Left err) -> return $ Left $ err
    (Right res) -> do
      a <- makeIoOnlyObj res
      return $ Right a
```
There are three things that should stand out in need of refactor:

1. the use of `sequence (map ..)`
2. manualling casing on the result of `sequence (map ..)`
3. binding monadically over Either only to perform another monadic (IO) action inside Either

The refactored version is much shorter:
```haskell
-- the function we're examining
pipelineFn :: Query
           -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn query = do
  a <- fetchFn query
  traverse makeIoOnlyObj (mapM decodeFn a)
  -- or traverse makeIoOnlyObj (traverse decodeFn a)
```
The `traverse decodeFn` is acting on Traversable list and Either monad, while `traverse makeIoOnlyObj` is acting on Traversable Either and IO monad.
Or we can go pointfree:
```haskell
pipelineFn =
  (traverse makeIoOnlyObj . traverse decodeFn =<<) . fetchFn
```

### 21.8 Do all the things
```haskell
module HttpStuff where

import Data.ByteString.Lazy hiding (map)
import Network.Wreq

urls :: [String]
urls = [ "http://httpbin.com/ip"
       , "http://httpbin.org/bytes/5"
       ]

mappingGet :: [IO (Response ByteString)]
mappingGet = map get urls

traversedUrls :: IO [Response ByteString]
traversedUrls = traverse get urls
```

#### Strength for understanding
Traversable is *stronger* than both Functor and Foldable. Watch how we can recover their functions:
```haskell
-- fmap
λ> import Data.Functor.Identity
λ> let fmap' f t = runIdentity $ traverse (Identity . f) t
λ> :t fmap'
fmap' :: Traversable t => (a -> b) -> t a -> t b
λ> fmap' (+1) [1..5]
[2,3,4,5,6]

-- foldMap
λ> import Data.Monoid
λ> import Data.Functor.Constant
λ> let foldMap' f t = getConstant $ traverse (Constant . f) t
λ> :t foldMap'
foldMap' :: (Monoid a, Traversable t) => (a1 -> a) -> t a1 -> a
```

### 21.9 Traversable instances
#### Either
```haskell
instance Traversable (Either a) where
  traverse _ (Left x)  = pure (Left x)
  traverse f (Right y) = Right <$> f y
```

#### Tuple
```haskell
instance Traversable ((,) a) where
  traverse f (x,y)  = (,) x <$> f y
```

### 21.10 Traversable Laws
The **traverse** function must satisfy the following:

1. Naturality

  ```haskell
  t . traverse f = traverse (t . f)
  ```
  Function composition behaves in unsurprising ways, whether within the traversed function or on the outer layer, once the traversed function has already generated the applicative structure.

2. Identity

  ```haskell
  traverse Identity = Identity
  ```
  Traversing the data constructor of Identity will produce the same result as just putting the value in Identity.
  Hence Identity represents a "structural" identity for traversing data.
  That is, the Traversable instance cannot add or inject any structure or unsuspecting "effects".

3. Composition

  ```haskell
  traverse (Compose . fmap g . f)
    = Compose . fmap (traverse g) . traverse f
  ```
  We can collapse sequential traversals into a single traversal

The **sequenceA** function must satisfy the following:

1. Naturality

  ```haskell
  t . sequenceA = sequenceA . fmap t
  ```

2. Identity

  ```haskell
  sequenceA . fmap Identity = Identity
  ```

3. Composition

  ```haskell
  sequenceA . fmap Compose
    = Compose . fmap sequenceA . sequenceA
  ```

### 21.11 Quality Control
See [QuickChecker.hs](./QuickChecker.hs) for examples quickchecking Traversable instances.
