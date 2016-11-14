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

### 9.9 Transforming lists of values
#### Exercises: More Bottoms
##### 1. What is the result of `take 1 $ map (+1) [undefined, 2, 3]`?
Blows up
##### 2. What is the result of `take 1 $ map (+1) [1, undefined, 3]`?
returns [2]
##### 4. What does this do? What is the type? `itIsMystery xs = map (\x -> elem x "aeiou") xs`
This takes a string and returns an array with a `Bool` value for each character depending on whether it is a vowel or not. The type is `itIsMystery :: [Char] -> [Bool]`.

### 9.10 Filtering lists of values
##### 1. Given the above, how might we write a filter function that would give us all the multiples of 3 out of a list from 1-30?
```haskell
filter ((==0) . (`mod`3)) [1..30] -- nice
```

##### 2. Recalling what we learned about function composition, how could we compose the above function with the length function to tell us *how many* multiples of 3 there are between 1 and 30?
```haskell
length $ filter ((==0) . (`mod`3)) [1..30] -- or
(length . filter ((==0) . (`mod`3))) [1..30]
```

##### 3. Make a function that removes all articles (’the’, ’a’, and ’an’) from sentences.
```haskell
filterArticles :: String -> [String]
filterArticles = filter (\w -> w /= "an" && w /= "a" && w /= "the") . words
```

### 9.11 Zipping lists
#### Zipping exercises
```haskell
-- 1. Write your own version of zip :: [a] -> [b] -> [(a, b)] and
--    ensure it behaves the same as the original.
zip' :: [a] -> [b] -> [(a, b)]
zip' _ []          = []
zip' [] _          = []
zip' (x:xs) (y:ys) = (x,y):(zip' xs ys)

-- 2. Do what you did for zip, but now for zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ _ []          = []
zipWith' _ [] _          = []
zipWith' f (x:xs) (y:ys) = (f x y):(zipWith' f xs ys)

-- 3. Rewrite your zip in terms of the zipWith you wrote.

zip'' :: [a] -> [b] -> [(a, b)]
zip'' = zipWith' (,)
```
