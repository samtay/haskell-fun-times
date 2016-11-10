# Types

### 5.3 Reading type signatures

#### Understanding the function type

`(->)` is the type constructor for functions. It takes arguments, but has no data constructors. See it compared to others:
```haskell
λ: :info (,)
data (,) a b = (,) a b  -- Defined in ‘GHC.Tuple’

λ: :info Bool
data Bool = False | True  -- Defined in ‘GHC.Types’

λ: :info (->)
data (->) a b   -- Defined in ‘GHC.Prim’
```

The tuple constructor needs two values to construct a tuple. A function similarly has two arguments (input & result), however there are no data constructors. The value at the term level is the function - *functions are values*.

The unique hallmark of a function is that it can be *applied*. The array type constructor is an *infix* operator associating to the right (even though application is left associative).

```haskell
fst :: (a, b)   ->   a
--       [1]    [2] [3]
```
1. First parameter has type `(a,b)`
2. The infix type constructor accepting two parameters: `(a,b)` and `a`
3. The result of type `a` -- the same `a` from `(a,b)`

That `a` is literally the same value, as the type signature shows us that nothing can possibly happen between input and output to that value with type `a`. Again -- formal explanation coming later on.

#### Exercises: Type Matching

- Functions:
 1. `not`
 1. `length`
 1. `concat`
 1. `head`
 1. `(<)`

- Type signatures:
 1. `_ :: [a] -> a`
 1. `_ :: [[a]] -> [a]`
 1. `_ :: Bool -> Bool`
 1. `_ :: [a] -> Int`
 1. `_ :: Ord a => a -> a -> Bool`

**i -> iii | ii -> iv | iii -> ii | iv -> i | v -> v**

### 5.4 Currying

Just like the lambda calculus, all Haskell functions are curried, meaning there are no functions that accept multiple arguments. There are just nested functions that intermediately return a function accepting a single parameter.

The way the type constructor `(->)` is defined makes currying the default. It is infix and right associative -- so
```haskell
f :: a -> a -> a
-- associates to
f :: a -> (a -> a)
```
which demonstrates currying very clearly. When we pass a single value to this function of type `a`, we get back a function of type `(a -> a)`.

#### Manual currying and uncurrying

*Uncurrying* means un-nesting functions by replacing nested functions with tuples. For example, to uncurry `(+)`, we'd define it as:
```haskell
uncAdd :: Num a => (a, a) -> a
uncAdd (x, y) = x + y
```

To automate this for functions with two args, we can define
```haskell
curry :: ((a, b) -> c) -> a -> b -> c
curry f a b = f (a, b)

uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f (a, b) = f a b
```
**TODO** Investigate automating this for all functions, for my own edification.

#### Sectioning

*Sectioning* is the partial application of infix operators which allows you to specify whether you are partially applying the first or second argument:
```haskell
Prelude> let x = 5
Prelude> let twoToThe = (2^)
Prelude> let square = (^2)
Prelude> twoToThe 5
32
Prelude> square 5
25
```

Utilizing higher order functions, this can be used on functions of any arity. For instance
```haskell
λ: let f a b c d = (a + b + c) - d
λ: let add10 = (10 `f`)
λ: let sub10 = (`f` 10)
```
will result in `add10` and `sub10` both of types `Num a => a -> a -> a -> a`. But `add10` will add `10` to the first two args and subtract the last arg, whereas `sub10` will add all three of its args and then subtract `10`.

#### Exercises: Type Arguments
Given a function and its type, tell us what type results from applying some or all of the arguments.

1. If the type of `f` is `a -> a -> a -> a`, and the type of `x` is `Char` then the type of `f x` is
 - [x] `Char -> Char -> Char`
 - [ ] `x -> x -> x -> x`
 - [ ] `a -> a -> a`
 - [ ] `a -> a -> a -> Char`
 
2. If the type of `g` is `a -> b -> c -> b`, then the type of `g 0 'c' "woot"` is
 - [ ] `String`
 - [ ] `Char -> String`
 - [ ] `Int`
 - [x] `Char`

3. If the type of `h` is `(Num a, Num b) => a -> b -> b`, then the type of `h 1.0 2` is
 - [ ] `Double`
 - [ ] `Integer`
 - [ ] `Integral b => b`
 - [x] `Num b => b`

9. If the type of `kessel` is `(Ord a, Num b) => a -> b -> a`, then the type of `kessel (1 :: Integer) 2` is
 - [ ] `Num a => a`
 - [ ] `Ord a => a`
 - [x] `Integer`
 - [ ] `(Num a, Ord a) => a`
 - [ ] `a`

### 5.5 Polymorphism

Type signatures may have three kinds of types: *concrete*, *constrained polymorphic*, or *parametrically polymorphic*.

**Constrained polymorphism** is what it sounds like -- constraining type variables via typeclasses such as `Num` or `Ord`

**Parametric polymorphism** is broader and refers to fully polymorphic type variables.

Constrained polymorphism decreases the number of concrete types a type variable can be, but increases what we can actually do with them by bringing into scope a set of operations from those typeclass constraints.

We can reason a *lot* about the type signatures of parametrically polymorphic functions. For instance `id :: a -> a` can do nothing other than return `a` because there is no information or method attached to its parameter at all -- nothing can be done *with* `a`. If instead it had a typeclass constraint such as `negate :: (Num a) -> a -> a`, now `a` has fewer concrete types that it could be, but there is now a set of functions that can be used on `a` at the term level, so things *can* be done with `a`.

Simply, if a variable can be *anything*, then there's very little that can be done to it because it has no methods. If it can be *some* types such as a type that is an instance of typeclass `Num`, then it has some methods.

#### Exercises: Parametricity

##### 2. We can get a more comfortable appreciation of parametricity by looking at `a -> a -> a`. This hypothetical function `a -> a -> a` has two–and only two–implementations. Write both possible versions of `a -> a -> a`.
```haskell
x :: a -> a -> a
x j k = j

y :: a -> a -> a
y j k = k
```

##### 3. Implement `a -> b -> b`. How many implementations can it have? Does the behavior change when the types of `a` and `b` change?
```haskell
x :: a -> b -> b
x j k = k
```
Just **1** implementation. Defined parametrically, the behavior stays the same for all types.

#### Polymorphic Constants

All values are given *maximum polymorphism*: `(-10)` has type `Num a => a`, which is as broad as Haskell can be in evaluating its type. This is called a *polymorphic constant* (**why?**). It will only resolve to a concrete type when it is forced to evaluate.

### 5.6 Type inference

Haskell’s type inference is an algorithm built on an extended version of the Damas-Hindley-Milner type system. Haskell will infer the most generally applicable (polymorphic) type that is still correct.

#### Exercises: Determine inferred types

```haskell
-- Type signature of general function
(++) :: [a] -> [a] -> [a]
-- How might that change when we apply
-- it to the following value?
myConcat x = x ++ " yo"
```
Changes to `[Char] -> [Char] -> [Char]`

```haskell
-- General function
(*) :: Num a => a -> a -> a
-- Applied to a value
myMult x = (x / 3) * 5
```
Changes to `(Fractional a) => a -> a -> a

```haskell
take :: Int -> [a] -> [a]
myTake x = take x "hey you"
```
Changes to `Int -> [Char] -> [Char]`

```haskell
(>) :: Ord a => a -> a -> Bool
myCom x = x > (length [1..10])
```
Changes to `Int -> Int -> Bool`

### 5.8 Chapter Exercises

#### Type-Kwon-Do
Manipulate terms in order to get the types to fit.

##### 1.
```haskell
f :: Int -> String
f = undefined

g :: String -> Char
g = undefined

h :: Int -> Char
h = g . f -- answer
```

##### 2.
```haskell
data A
data B
data C

q :: A -> B
q = undefined

w :: B -> C
w = undefined

e :: A -> C
e = w . q -- answer
```

##### 4.
```haskell
munge :: (x -> y) -> (y -> (w, z)) -> x -> w
munge xToy yTowz a = fst $ yTowz (xToy a)
```
