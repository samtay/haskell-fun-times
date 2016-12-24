# Monad
The most popular topic in Haskell.

### 18.1 Monad
Monads are not necessary to Haskell.
While the current standard of the language uses monads for constructing and transforming `IO` actions, older versions did not.
Monads are fun, but they do not define Haskell.

### 18.2 Sorry --- Monad is not a burrito
The monad typeclass is defined as:
```haskell
class Applicative m => Monad m where
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  return :: a -> m a
```
These are the core operations, however only `(>>=)` is necessary for a complete minimal instance definition.
`return` is not very interesting - it is just like `pure` from Applicative.
`>>` is called the sequencing operator, because it sequences two actions, discarding any result from the first action.
`>>=` is called bind, and is what really makes the monad special.
Even the sequencing action `>>` has an Applicative counterpart not yet discussed.

#### Applicative m
The Applicative superclass of Monad is actually a more recent update to GHC.
Just like how Applicative is stronger than Functor, we can derive both Functor and Applicative instances from Monad, for instance:
```haskell
fmap f xs = xs >>= return . f
```
As we'll see, this is actually a law of monads.

#### The novel part of monad
The bind function on its own isn't all that magical or surprising. The type is very similar to `fmap` and `<*>`. In fact, we can specialize the `fmap` and get something kind of similar:
```haskell
-- fmap :: (a -> b)   -> f a -> f b
-- fmap :: (a -> f b) -> f a -> f (f b)
λ> let andOne x = [x, 1]
λ> andOne 10
[10,1]

λ> fmap andOne [4, 5, 6]
[[4,1],[5,1],[6,1]]
```
Except now we have the extra layer of nesting that we saw from the type `f (f b)`.
For lists, we know that `concat` can solve that:
```haskell
λ> concat $ fmap andOne [4, 5, 6]
[4,1,5,1,6,1]
```
So, at least for lists, we have accomplished bind `>>=` via `fmap` and `concat`.
As it turns out, Monad is in a sense a generalization of concat.
The unique part of Monad is the `join` function:
```haskell
join :: Monad m => m (m a) -> m a
```

#### Exercise: Write `bind` in terms of `fmap` and `join`
```haskell
bind :: Monad m => (a -> m b) -> m a -> m b
bind f = join . fmap f
```

#### What Monad is not
Many people approach monad from one or two intuitive perspectives, such as restricting their thoughts to the `IO Monad`.
This leads to limited intuitions for monads and often misconceived notions of what `IO` is really about.
Luckily, I'm a mathematician, and don't fall for cheap tricks. A monad is precisely what its algebriac definition specifies. Regardless, here are notes of avoiding common pitfalls: a monad is **not**

1. Impure. All monadic functions are pure functions. `IO` is a datatype that allows for impure actions, and it has a `Monad` instance, but monads themselves are not impure.
2. An embedded language for imperative programming. Some monads are often used for sequencing actions, but there are plenty of commutative monads that do not order actions.
3. A value.
4. About strictness. The monadic operations `bind` and `return` are nonstrict.

#### Monad also lifts!
 The `Monad` class includes a set of `lift` functions that are the same as those from `Applicative`, but have been around since before applicatives were discovered.
```haskell
liftA :: Applicative f => (a  -> b) -> f a  -> f b
liftM :: Monad m =>       (a1 -> r) -> m a1 -> m r
```

### 18.3 Do syntax and monads
First, consider this function which exists for both applicatives and monads:
```haskell
(*>) :: Applicative f => f a -> f b -> f b
(>>) :: Monad m =>       m a -> m b -> m b
```
Other than the constraints, these should always do the same thing: sequence functions.

Now, consider some do/non-do operations that accomplish the same thing:
```haskell
-- sequencing
sequencing :: IO ()
sequencing = do
  putStrLn "blah"
  putStrLn "another thing"
sequencing' :: IO ()
sequencing' =
  putStrLn "blah" >>
  putStrLn "another thing"
sequencing'' :: IO ()
sequencing'' =
  putStrLn "blah" *>
  putStrLn "another thing"

-- binding
binding :: IO ()
binding = do
  name <- getLine
  putStrLn name
binding' :: IO ()
binding' =
  getLine >>= putStrLn
```

### When fmap alone isn't enough
Notice that this doesn't work:
```haskell
λ> putStrLn <$> getLine
```
or at least, it doesn't output anything.
```haskell
λ> :t putStrLn <$> getLine
IO (IO ())
```

To see why printing doesn't work here, first let's note one of Haskell's great strengths: we can refer to, compose, and map over effectful computations without performing them. Such as:
```haskell
Prelude> let printOne = putStrLn "1"
Prelude> let printTwo = putStrLn "2"
Prelude> let twoActions = (printOne, printTwo)
Prelude> fst twoActions
1
Prelude> snd twoActions
2
Prelude> fst twoActions
1
```

So, returning to getting and printing the line:
1. First `getLine` performs `IO` to get a `String` resulting in `IO String`.
2. And `putStrLn` takes a `String` argument, performs `IO`, and returns "nothing": `String -> IO ()`.
3. Thus `putStrLn <$> getLine :: IO (IO ()) `

Because the result of `putStrLn` is wrapped in an extra `IO ()` layer, its effects do not take place, similar to us nesting print actions in the tuples above. Of course, `join` will fix this:
```haskell
Prelude> import Control.Monad (join)
Prelude> join $ putStrLn <$> getLine
blah
blah
Prelude> :t join $ putStrLn <$> getLine
join $ putStrLn <$> getLine :: IO ()
```
Here, `join` merged the effects of these two functions into a single `IO` action - this merged `IO` action performs the effects in the "order" determined by the nesting of `IO` actions -- the definition of `join`! The cleanest way to express "ordering" in a lambda calculus is through nesting of expressions.

Back to equating do/non-do notation:
```haskell
bindingAndSequencing :: IO ()
bindingAndSequencing = do
  putStrLn "name pls:"
  name <- getLine
  putStrLn ("y helo thar: " ++ name)
bindingAndSequencing' :: IO ()
bindingAndSequencing' =
  putStrLn "name pls:" >>
  getLine >>=
  \name -> putStrLn ("y helo thar: " ++ name)

twoBinds :: IO ()
twoBinds = do
  putStrLn "name pls:"
  name <- getLine
  putStrLn "age pls:"
  age <- getLine
  putStrLn ("y helo thar: "
            ++ name ++ " who is: "
            ++ age ++ " years old.")
twoBinds' :: IO ()
twoBinds' =
  putStrLn "name pls:" >>
  getLine >>=
  \name ->
  putStrLn "age pls:" >>
  getLine >>=
  \age ->
  putStrLn ("y helo thar: "
            ++ name ++ " who is: "
            ++ age ++ " years old.")
```
Makes you appreciate `do` syntax.

### 18.4 Examples of Monad use
#### List
```haskell
twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
  x <- xs
  if even x
    then [x*x, x*x]
    else [x*x]

λ> twiceWhenEven [1..3]
[1,4,4,9]
```

#### Maybe
For brevity, I'll just include the types of `Cow` and its validation methods:
```haskell
data Cow = Cow {
    name :: String
  , age :: Int
  , weight :: Int
  } deriving (Eq, Show)
noEmpty :: String -> Maybe String
noNegative :: Int -> Maybe Int
weightCheck :: Cow -> Maybe Cow
-- Now when making, let's use pretty do syntax:
mkSphericalCow :: String -> Int -> Int -> Maybe Cow
mkSphericalCow' name' age' weight' = do
  nammy <- noEmpty name'
  agey <- noNegative age'
  weighty <- noNegative weight'
  weightCheck (Cow nammy agey weighty)
```

Note that we could **not** accomplish this with an Applicative, because `weightCheck` needs a `Cow` instance and returns more monadic structure. In general,
```haskell
-- This can be rewritten using Applicative
doSomething = do
  a <- f
  b <- g
  c <- h
  pure (a, b, c)
-- This requires monad
doSomething' n = do
  a <- f n
  b <- g a
  c <- h b
  pure (a, b, c)
```
The second example needs a `Monad` instance because `g` and `h` produce monadic structure, based on values (`a`, `b`) that can only be obtained by depending on values generated by monadic structure (`f n`, `g a`).

### Either
Just going to include the type signatures from the `EitherMonad` example:
```haskell
module EitherMonad where

-- years ago
type Founded = Int

-- number of programmers
type Coders = Int

data SoftwareShop =
  Shop {
      founded :: Founded
    , programmers :: Coders
  } deriving (Eq, Show)

data FoundedError =
    NegativeYears Founded
  | TooManyYears Founded
  | NegativeCoders Coders
  | TooManyCoders Coders
  | TooManyCodersForYears Founded Coders
  deriving (Eq, Show)

-- validating methods
validateFounded :: Int -> Either FoundedError Founded -- year errors
validateCoders :: Int -> Either FoundedError Coders   -- coder errors

-- make softwareshop
mkSoftware :: Int -> Int -> Either FoundedError SoftwareShop
mkSoftware years coders = do
  founded     <- validateFounded years
  programmers <- validateCoders coders
  if programmers > div founded 10
    then Left $ TooManyCodersForYears founded programmers
    else Right $ Shop founded programmers
```
**Note** `Either` always short-circuits on the *first* failure.
This is important because in the monad, later values can depend on previous ones.
So there is no `Monad` instance for `Validation`, since `Applicative` and `Monad` instances **must** have the same behavior.
That is, the Applicative apply functionality must not change behavior when derived from the Monad instance's bind operation:
```haskell
import Control.Monad (ap)
(<*>) == ap

-- where
ap :: (Monad m) => m (a -> b) -> m a -> m b
ap g m = do
  f <- g
  x <- m
  return (f x)
```
So if we tried to make a `Monad` instance for `Validation`, we'd end up with one identical to `Either`'s.

#### Exercise: Implement the Either Monad
```haskell
module EitherMonad where

import Control.Applicative (liftA)

data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap = liftA

instance Applicative (Sum a) where
  pure      = return
  (<*>) f x = x >>= (\x' -> f >>= (\f' -> return $ f' x'))

instance Monad (Sum a) where
  return         = Second
  First x >>= _  = First x
  Second y >>= f = f y
```
