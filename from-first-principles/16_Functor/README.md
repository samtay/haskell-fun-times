# Functor

### 16.2 What's a functor?
A functor is a way to apply a function over or around some structure that we don't want to alter.
Funtor is implemented in Haskell with a typeclass:
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
(.)  :: (b -> c) -> (a -> b) -> a -> c

-- and for Functors f, g
fmap :: (a -> b)     -> f a   -> f b
fmap :: (f a -> f b) -> g f a -> g f b

-- recalling that functions associate to the right, composing higher order functions
-- leaves them partially applied: a -> z -> y will compose as a -> (z -> y)
-- hence
-- a ~> a -> b and b ~> f a -> f b
fmap :: (a -> b)                    -- as first arg to (.)
     :: ((a -> b) -> (f a -> f b)
     :: (b -> c)                    -- as second arg to (.)
     :: ((f a -> f b) -> (g f a -> g f b))

-- therefore
fmap . fmap :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
```

### 16.8 Transforming the unapplied type argument
Note that for kinds greater than `* -> *`, such as `Either a b` and `(a,b)`, we can only use Functor to act on the last element.
Trying to act on both elements will not typecheck or pass Functor laws.
This is fairly immediate, given that we need to accept a function of type `a -> b`; how could we generically apply this to both arguments, which can be of different types?

Knowing then that we must partially the type contructors and have Functor instances such as `Either a` and `(,) a`, it is clear that trying to modify the value of type `a` will fail Functor laws, as that will mess with the "structure" which includes the type `a`.

### 16.12 A somewhat surprising functor
Recall the `const` function: `const :: a -> b -> a`.
Similar to this, there is a `Constant` datatype:
```haskell
newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Eq, Show)
```

Here `b` is a *phantom* type, as it has no corresponding witness at the value/term level.
Since we need to partially apply the first argument to get kind `* -> *`, the functor has to work on the phantom type:
```haskell
instance Functor (Constant m) where
  fmap _ (Constant v) = Constant v
```
Now we can `fmap` over this and always get the same thing back, no matter what function we are mapping.

### 16.13 More structure, more functors
```haskell
data Wrap f a =
  Wrap (f a)
  deriving (Eq, Show)

instance Functor f => Functor (Wrap f) where
  fmap f (Wrap fa) = Wrap (fmap f fa)
```
The only way to work out these types and get a result `Wrap f b` from `f :: a -> b` is to "access" and apply `f` to the inner `a`. Hence we must impose a Functor constraint on `f`.

### 16.14 IO Functor
An initial glance at `IO`.
Note that `IO` is an abstract datatype; there are no data constructors that we can match on.
This means that its typeclasses are the only way we can work with values of type `IO a`. Functor is one such typeclass:
```haskell
λ> fmap (++ " world") getLine
hello
"hello world"
```

### 16.15 What if we want something different?
As discussed earlier, Functors are used to to lift functions over structure, so that we can transform only the contents and not the structure.
What about the opposite?
Transform only the structure and leave the type argument alone? Something like
```haskell
nat :: (f -> g) -> f a -> g a
```

This is known as a *natural transformation*. First, note that the above type signature is impossible: `f` and `g` are higher-kinded types as indicated by the `f a` and `g a`, so they can't appear "alone" as `f` and `g` respectively in a function type signature.

We can use a language pragma to allow this sort of thing:
```haskell
{-# LANGUAGE RankNTypes #-}

type Nat f g = forall a. f a -> g a
```
Note: the quantification `forall` of type `a` forces all functions of this type to be oblivious to the contents of the structures `f` and `g`. This is precisely what we want for a natural transformation. Without the language pragma, we could define something like `type Nat f g a = f a -> g a`, however this will not be as strict; the compiler will let us "mess" with the `a` value, and for a natural transformation, we'd rather it yell at us.

We'll return to natural transformations later.

### 16.16 Functors are unique to a datatype
In Haskell, Functor instances are unique for a given datatype, unlike Monoids. This is due to parametricity and because arguments to type constructors are applied in the order they are defined. The proof is sadly omitted...

### 16.17 Chapter Exercises
#### Determine if the following can have Functor instances:

##### 1. `data Bool = False | True`
No, `Bool` has kind `*`.

##### 2. `data BoolAndSomethingElse a = False' a | True' a`
Yes

##### 3. `data BoolAndMaybeSomethingElse a = Falsish | Truish a`
Yes, this is isomorphic to `Maybe`.

##### 4. `newtype Mu f = InF { outF :: f (Mu f) }`
I think so.. the kind is correct. Let's try:
```haskell
instance Functor Mu where
  fmap g (InF fx) = InF $ fmap g fx

-- No, that fails because Mu has type (* -> *) -> *

instance Functor f => Functor Mu f where
  fmap g (InF fx) = InF $ fmap g fx
```
No, this doesn't work either.
Actually after playing in the REPL I think this is impossible.
The problem is that `Mu` has type `(* -> *) -> *`; on its own, `Mu` clearly cannot be a Functor.
However, if we partially apply it as `M f` we have to pass it a type of kind `* -> *`, and then we are left with kind `*`.
Thus, there is no way to form a type `* -> *` for the functor instance.

##### 5. `data D = D (Array Word Word) Int Int`
No, `D` has kind `*`.

#### Write Functor instances for the following datatypes.
```haskell
-- 1
data Quant a b = Finance | Desk a | Bloor b

instance Functor (Quant a) where
  fmap f Finance   = Finance
  fmap f (Desk a)  = Desk a
  fmap f (Bloor b) = Bloor (f b)

-- 2
data K a b = K a

instance Functor (K a) where
  fmap _ (K a) = K a

-- 3
{-# LANGUAGE FlexibleInstances #-}
newtype Flip f a b =
  Flip (f b a)
  deriving (Eq, Show)

newtype K a b = K a

instance Functor (Flip K a) where
  fmap f (Flip (K b)) = Flip $ K (f b)

-- 4
data EvilGoateeConst a b =
  GoatyConst b

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst x) = GoatyConst $ f x

-- 5
data LiftItOut f a =
  LiftItOut (f a)

instance Functor f => Functor (LiftItOut f) where
  fmap g (LiftItOut fx) = LiftItOut $ fmap g fx

-- 6
data Parappa f g a =
  DaWrappa (f a) (g a)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap z (DaWrappa fx gx) = DaWrappa (fmap z fx) (fmap z gx)

-- 7
data IgnoreOne f g a b =
  IgnoringSomething (f a) (g b)

instance Functor g => Functor (IgnoreOne f g a) where
  fmap z (IgnoringSomething fx gx) = IgnoringSomething fx (fmap z gx)

-- 9
data List a = Nil | Cons a (List a)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

-- 10
data GoatLord a =
    NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)

instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat x) = OneGoat (f x)
  fmap f (MoreGoats a b c) =
    MoreGoats (fmap f a) (fmap f b) (fmap f c)

-- 11
data TalkToMe a =
    Halt
  | Print String a
  | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print s x) = Print s (f x)
  fmap f (Read g) = Read $ f . g
```
