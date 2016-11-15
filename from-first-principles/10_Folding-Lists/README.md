# Data structure origami
Similar to `reduce` in other languages, but way more badass due to laziness. Note that GHC 7.10 abstracted out the list-specific part of folding to a more reusable `Foldable` typeclass.

### 10.3 Recursive patterns
Notice the pattern of these definitions:
```haskell
sum :: [Integer] -> Integer
sum []     = 0
sum (x:xs) = x + sum xs

length :: [a] -> Integer
length []     = 0
length (_:xs) = 1 + length xs

-- and [myOr, myAny, myElem](../9_Lists/MyStandard.hs)
```
- the base case is the identity for the function
- each has a main function with a recursive pattern that *associates to the right* - the head of the list gets evaluated, set aside, and then the function moves to the right.

Enter the fold:
```haskell
-- simple definition
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z []     = z
foldr f z (x:xs) = f x (foldr f z xs)
```

### 10.4 Fold right
```haskell
foldr (+) 0 [1, 2, 3] =
  case [1, 2, 3] of
    []           -> 0
    (1 : [2, 3]) -> (+) 1 (foldr (+) 0 [2, 3])
```
Note that `(foldr (+) 0 [2, 3])` is evaluated **only** because `(+)` is strict in both of its arguments. However, see what happens with `const`:
```haskell
foldr const 0 [1, undefined] =
  case [1, undefined] of
    []                -> 0
    (1 : [undefined]) -> const 1 (foldr const 0 [undefined])

-- Since const x _ = x, the rest of the fold (spine + folding) is never evaluated.
```

Folding happens in two stages: traversal and folding. Traversal is the stage in which the fold recurses over the spine. Folding refers to the evaluation or reduction of the folding function applied to the values. The spine is always recursed in the same direction, but left/right folds differ in association of the folding function, and hence which direction the folding, or reduction, proceeds.

Given this two-stage process and non-strict evaluation, if `f` doesn’t evaluate its second argument (rest of the fold), no more of the spine will be forced -- so this can even be used for infinite lists, like we do with `take`.

The one part of this that is always strict is the pattern match on the first cons cell, discriminating the `[]` and `(x:xs)` cases. So the first cons cell *cannot* be undefined.
```haskell
-- undefined value is never evaluated
Prelude> foldr (\_ _ -> 9001) 0 [1, 2, 3, undefined]
9001

-- the cons cell are not bottom, just contain bottom !
Prelude> foldr (\_ _ -> 9001) 0 [undefined, undefined]
9001

-- undefined cons cells of the spine are never evaluated
Prelude> foldr (\_ _ -> 9001) 0 ([1, 2, 3] ++ undefined)
9001

-- initial cons cell is undefined !
Prelude> foldr (\_ _ -> 9001) 0 (undefined ++ [1, 2, 3])
*** Exception: Prelude.undefined
```

### 10.5 Fold left
The left fold can be defined as:
```haskell
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f acc []     = acc
foldl f acc (x:xs) = foldl f (f acc x) xs
```
To see the difference that association makes, we need to use a function that isn't associative:
```haskell
foldr (^) 2 [1..3]
(1 ^ (2 ^ (3 ^ 2)))
(1 ^ (2 ^ 9))
1 ^ 512
1
-- while
foldl (^) 2 [1..3]
((2 ^ 1) ^ 2) ^ 3
(2 ^ 2) ^ 3
4 ^ 3
64
```
Note that `scanr` and `scanl` are parallel to their fold functions, but return a list of intermediate values from the folding function, such that
```haskell
last (scanl f z xs) = foldl f z xs
head (scanr f z xs) = foldr f z xs
```

#### Unconditional spine recursion
It is important that the left fold has the accumulated steps of the fold as its first argument, not intermediated by the folding function as it is in `foldr`:
```haskell
foldr f acc (x:xs) = f x (foldr f acc xs)
foldl f acc (x:xs) = foldl f (f acc x) xs
```
This means that no matter what the folding function is, **`foldl` always recurses the entire spine**, unlike the laziness we saw at play with `foldr const`. This means that all cons cells must be well defined. However, values in the cons cells can still be undefined depending on the folding function:
```haskell
λ> foldl (\_ _ -> 5) 0 ([1..5] ++ undefined)
*** Exception: Prelude.undefined
λ> foldl (\_ _ -> 5) 0 ([1..5] ++ [undefined])
5
```

### Exercises
##### 2. What does the following mystery function do?
```haskell
seekritFunc x =
  div (sum (map length (words x)))
    (length (words x))
```
This gives the average length of word in a sentence (rounded down because `div` is integral).
