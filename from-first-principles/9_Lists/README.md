# Lists

Lists serve two purposes:
- as collection/purality of values
- as infinite series acting as stream datatype

### 9.2 The list datatype
```haskell
data [] a = [] | a : [a]
```
- So `[]` is the type constructor for lists and the data constructor for the empty list.
- The `[]` data constructor is a *nullary* constructor as it takes no arguments.
- The `:` data constructor is an infix operator called *cons*, taking a value of type `a` and a list of type `[a]` (recursive definition).
- The list datatype itself is a sum type, but the data constructor `:` is a product type since it takes two arguments.

### 9.4 List's syntactic sugar
Lists are often discussed in terms of *cons cells* and *spines*. The cons cell is the conceptual space that values may inhabit, while the spine is the connectice structure holding the cons cells together and in place. This structure actually nests cons cells rather them ordering them horizontally. The syntactic sugar for `(1 : 2 : 3 : []) == [1,2,3,4]` obscures this construction, but diferent functions treat the spine / cons cells differently, so it's important to keep in mind.

### 9.5 Using ranges to construct lists
```haskell
λ> [1..10]
[1,2,3,4,5,6,7,8,9,10]

λ> [3..10]
[3,4,5,6,7,8,9,10]

-- Using three numbers [x,y..z] will go from x to z, in gaps of (y - z)
λ> [2,4..10]
[2,4,6,8,10]

λ> [1,3..10]
[1,3,5,7,9]
```
Underlying functions of range syntax are:
```haskell
-- infinite starting at x
-- [x..]
enumFrom :: Enum a => a -> [a]

-- infinite  starting at x in gaps of y - x
-- [x,y..]
enumFromThen :: Enum a => a -> a -> [a]

-- finite from x to z
-- [x..z]
enumFromTo :: Enum a => a -> a -> [a]

-- finite from x to z in gaps of (y - x)
-- [x,y..z]
enumFromThenTo :: Enum a => a -> a -> a -> [a]
```

### 9.7 List comprehensions
My favorite. Just keep in mind multiple lists are exhausted from the right first:
```haskell
[x^y | x <- [1..3], y <- [2, 3]]
-- = [1^2, 1^3, 2^2, 2^3, 3^2, 3^3]
[(x,y) | x <- [1,2], y <- ['a','b']]
-- = [(1,'a'), (1,'b'), (2,'a'), (2,'b')]
```

#### Exercises: Square Cube
Given the following:
```haskell
Prelude> let mySqr = [x^2 | x <- [1..5]]
Prelude> let myCube = [y^3 | y <- [1..5]]
```
##### 1. First write an expression that will make tuples of the outputs of `mySqr` and `myCube`.
```haskell
[(s,c) | s <- mySqr, c <- myCube]
```
##### 2. Now alter that expression so that it only uses the `x` and `y` values that are less than 50.
```haskell
[(s,c) | s <- mySqr, c <- myCube, s < 50, c < 50]
```
##### 3. Apply another function to that list comprehension to determine how many tuples inhabit your output list.
```haskell
length [(s,c) | s <- mySqr, c <- myCube, s < 50, c < 50]
```

### 9.8 Spines and nonstrict evaluation
When we talk about data structures in Haskell, particularly lists, sequences, and trees, we talk about them having a **spine**.
```
   : <------|
  / \       |
_    : <----| This is the "spine"
    / \     |
   _   : <--|
      / \
     _   []
```
```haskell
1 : (2 : (3 : []))
```
Note that `1` is not *before* the cons cell `(:)`; the cons cells contain the value. Because of this and the way nonstrict evaluation works, you can evaluate cons cells independently of what they contain.

Evaluation goes *down* the spine from `1` to `3` while construction goes *up* the spine from `3` to `1`.

Remember that due to laziness, the list isn't constructed until consumed. Up until then, it's just a series of placeholders. This is easily verified using `:spine` in GHCi.

#### Spines are evaluated independently of values
Values in Haskell get reduced to weak head normal form by default, which is just enough evaluation necessary to reach a data constructor or await a function argument.
```haskell
\x -> x * 10 -- WHNF & NF
(1, 1 + 1) -- WHNF & ! NF
"Papu" ++ "chon" -- ! WHNF & ! NF
(1, "Papu" ++ "chon") -- WHNF & ! NF
```
However, functions that are *spine strict* can force complete evaluation of the spine of the list even if they don’t force evaluation of each value. Pattern matching is strict by default, so pattern matching on cons cells can mean forcing spine strictness if your function doesn’t stop recursing the list. For example `length` is strict in the spine but not the values:
```haskell
λ> length [1, undefined, 3]
3
```

A common mantra for performance sensitive code in Haskell is **lazy in the spine, strict in the leaves**.

#### Bottom Madness: will it blow up?
##### 2. `take 1 $ [x^y | x <- [1..5], y <- [2, undefined]]`
No
##### 3. `sum [1, undefined, 3]`
Yes
##### 4. `length [1, 2, undefined]`
No
##### 5. `length $ [1, 2, 3] ++ undefined`
Yes
##### 6. `take 1 $ filter even [1, 2, 3, undefined]`
No! Interesting, `filter` is lazy as well.. or at least contextually knows that it is being applied before `take`?
##### 7. `take 1 $ filter even [1, 3, undefined]`
Yes
##### 8. `take 1 $ filter odd [1, 3, undefined]`
No
##### 9. `take 2 $ filter odd [1, 3, undefined]`
No
##### 10. `take 3 $ filter odd [1, 3, undefined]`
Yes

#### Intermission: Is it in normal form?
1. `[1, 2, 3, 4, 5]`

  NF
2. `1 : 2 : 3 : 4 : _`

  WHNF
3. `enumFromTo 1 10`
 
  Neither
4. `length [1, 2, 3, 4, 5]`

  Neither
5. `sum (enumFromTo 1 10)`

  Neither
6. `['a'..'m'] ++ ['n'..'z']`

  Neither
7. `(_, 'b')`

  WHNF

I say neither for the above cases because they have functions (that are not data constructors) with arguments applied yet not evaluated.
