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
