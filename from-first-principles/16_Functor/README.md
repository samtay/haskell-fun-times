# Functor

### 16.2 What's a functor?
A functor is a way to apply a function over or around some structure that we don't want to alter. Funtor is implemented in Haskell with a typeclass:
```haskell
class Functor f where
  fmap :: (a->b) -> f a -> f b
```

### 16.4 Let's talk about *f*, baby
We can deduce that `f` must have kind `* -> *` by looking at the type signature of `fmap`; each argument must have kind `*` (because each argument/return must be inhabitable); since `f a` and `f b` both exist as single arguments in the type signature, we know that `f a` and `f b` both have kind `*`, so `f` has kind `* -> *`.

Thus, the following are impossible:
```haskell
class Impish v where
  impossibleKind :: v -> v a

class AlsoImp v where
  nope :: v a -> v
```

#### Exercises: Be Kind
Given a type signature, determine the kinds:

1. `a -> a`

 `a :: *`
2. `a -> b a -> T (b a)`

  `a :: *`
  `b :: * -> *`
  `T :: * -> *`

3. `c a b -> c b a`

  `c :: * -> * -> *`

#### Functor is function application
Functors simply allow us to apply functions within higher kinded types. Notice the similarities:
```haskell
(<$>) :: Functor f => (a -> b) -> f a -> f b
($)   ::              (a -> b) ->   a ->   b
```
This shows how a functor is a way to apply functions over/through/past some structure `f` that we want to leave untouched. (The leaved untouched bit is where functor laws come into play.)
### 16.5 Functor Laws
```haskell
# 1. Identity
fmap id == id

# 2. Composition
fmap (f . g) == fmap f . fmap g
```

The law of identity is ensuring that we don't mess with the structure around the values. The law of composition concerns, well, composability. Acting on the values within a structure via two consecutive operations `g` and `f` should be the same whether they are composed via single `fmap` operation or if they are each `fmap`-ed.

The recurring theme is that we **leave the structure untouched**. The whole purpose of this typeclass is to reuse functions in the presence of more structure and be transparently oblivious to that structure. Don't break the laws. If you want to change the structure around a value, just use a normal function!

**Note** that the composition law is redundant... #1 => #2. But, it's an important thing to keep in mind.

### 16.7 Commonly used functors
#### The functors are stacked and that's a fact
Let's take a look at nested structures
```haskell
>>> let lms = [Just "ave", Nothing, Just "woohoo"]
>>> let replaceWithP = const 'p'

>>> replaceWithP lms
'p'

>>> fmap replaceWithP lms
['p', 'p', 'p']

>>> (fmap . fmap) replaceWithP lms
[Just 'p', Nothing, Just 'p']

>>> (fmap . fmap . fmap) replaceWithP lms
[Just "ppp", Nothing, Just "pppppp"]
```

Very cool. Here we are composing `fmap` to access structures within structures.
Playing in GHCi we can verify how this works:
```haskell
(,)  :: (b -> c) -> (a -> b) -> a -> c

-- and for Functors f, g
fmap :: (a -> b)     -> f a   -> f b
fmap :: (f a -> f b) -> g f a -> g f b

-- recalling that functions associate to the right, composing higher order functions
-- leaves them partially applied: a -> z -> y will compose as a -> (z -> y)
-- hence
-- a ~> a -> b and b ~> f a -> fb
fmap :: (a -> b)                    -- as first arg to (.)
     :: ((a -> b) -> (f a -> f b)
     :: (b -> c)                    -- as second arg to (.)
     :: ((f a -> f b) -> (g f a -> g f b))

-- therefore
fmap . fmap :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
```

### 16.8 Transforming the unapplied type argument
Note that for kinds greater than `* -> *`, such as `Either a b` and `(a,b)`, we can only use Functor to act on the last element. Trying to act on both elements will not typecheck or pass Functor laws. This is fairly immediate, given that we need to accept a function of type `a -> b`; how could we generically apply this to both arguments, which can be of different types?

Knowing then that we must partially the type contructors and have Functor instances such as `Either a` and `(,) a`, it is clear that trying to modify the value of type `a` will fail Functor laws, as that will mess with the "structure" which includes the type `a`.

### 16.9 QuickChecking Functor instances
