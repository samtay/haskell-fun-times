# Recursion
**Recursion** is defining a function in terms of itself via self-referential expressions. But the lambda calculus does not appear on the surface to have any means of recursion, because of the anonymity of expressions. Being able to write recursive functions, though, is essential to Turing completeness.

We use a combinator – known as the *Y combinator* or fixed-point combinator – to write recursive functions in the lambda calculus.  Haskell has native recursion ability based on the same principle as the Y combinator. [[Reference]](http://mvanier.livejournal.com/2897.html)

#### Another way to look at recursion
Recall composition
```haskell
(.) :: (b -> c) -> (a -> b) -> a -> c
```
Recursion is simply *self-referential composition*. The difference between recursion and `(.)` is that instead of a fixed number of function applications, recursivce functions rely on inputs to determine when to stop applying functions to successive results.

### 8.3 Bottom
⊥ or **bottom** is a term used in Haskell to refer to computations that do not successfully result in a value. The main forms of bottom are computations that failed with an error or those that failed to terminate.
```haskell
-- Exception: blah
f :: Bool -> Int
f True = error "blah"
f False = 0

-- Exception: Non-exhaustive patterns in function f
f :: Bool -> Int
f False = 0
```

### 8.5 Integral division from scratch
```haskell
dividedBy :: Integer -> Integer -> Integer
dividedBy num denom = fst $ go num denom 0
  where go n d i
          | n < d     = (i, n)
          | otherwise = go (n - d) d (i + 1)
```
This `go` function is a common Haskell idiom to define an intermediary function in a `where` clause to accept more arguments than the top level function -- here, this is done to keep track of the recursion count.

### 8.6 Chapter Exercises
Located  in `.hs` files of this directory. See [WordNumber](./WordNumber.hs) for the first nontrivial exercise of this book.
