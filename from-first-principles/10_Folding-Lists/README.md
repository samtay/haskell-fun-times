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

### 10.4 Fold right
```haskell
foldr (+) 0 [1, 2, 3] =
  case [1, 2, 3] of
    []           -> 0
    (1 : [2, 3]) -> (+) 1 (foldr (+) 0 [2, 3])
```
Note that `(foldr (+) 0 [2, 3])` is evaluated **only** because `(+)` is strict in both of its arguments. However, see what happens with `const`:

# TODO put const laziness example here

Folding happens in two stages: traversal and folding. Traversal is the stage in which the fold recurses over the spine. Folding refers to the evaluation or reduction of the folding function applied to the values. The spine is always recursed in the same direction, but left/right folds differ in association of the folding function, and hence which direction the folding, or reduction, proceeds.
