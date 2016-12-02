# More Functional Patterns

### 7.2 Arguments and parameters

#### Binding variables to values
Binding of variables can occur through function application, and `let` and `where` expressions. Parameters are bound to the values applied to a function throughout the entire scope of the function body, while variables bound by `let` and `where` expressions are only in scope *inside* the given expression.

In some cases, function arguments are not visible in the function if they have been **shadowed**:
```haskell
bindExp :: Integer -> String
bindExp x = let x = 10; y = 5 in
  "the integer was: " ++ show x
  ++ " and y was: " ++ show y
```
The reference to `x` arising from the argument `x` is shadowed by the `x` in the `let` binding, because the definition of `x` that is "innermost" in the code takes precedence. This is because Haskell is **lexically scoped**, meaning that resolving a value for a variable depends on the location in the code and the lexical context, for example in `let` and `where` clauses.

#### Exercises: Grab Bag
##### 1. Which (two or more) of the following are equivalent?
 - [x] `mTh x y z = x * y * z`
 - [x] `mTh x y = \z -> x * y * z`
 - [x] `mTh x = \y -> \z -> x * y * z`
 - [x] `mTh = \x -> \y -> \z -> x * y * z`

##### 2. The type of `mTh` (above) is `Num a => a -> a -> a -> a`.  Which is the type of `mTh 3`?
 - [ ] `Integer -> Integer -> Integer`
 - [ ] `Num a => a -> a -> a -> a`
 - [ ] `Num a => a -> a`
 - [x] `Num a => a -> a -> a`

#### Utility of lambda syntax
**Keep in mind** that named entities and anonymous entities evaluate differently in Haskell: **details to follow**.

### 7.4 Pattern matching
Remember **always handle all cases**. Partial functions are a **no go**. Eventually, they will fail and return *bottom*, a non-value that will result in an exception. Remember `-Wall` GHCi setting will help catch these.

### 7.5 Case expressions
#### Exercises: Case Practice
##### 1. Rewrite `functionC x y = if (x > y) then x else y` using case expressions
```haskell
functionC x y = case (x > y) of
  True  -> x
  False -> y
```

##### 2. Rewrite `ifEvenAdd2 n = if even n then (n+2) else n` using case expressions
```haskell
ifEvenAdd2 n = case (even n) of
  True  -> n + 2
  False -> n
```

##### 3. Fix the following function to cover all cases
```haskell
nums x =
  case compare x 0 of
    LT -> -1
    GT -> 1
    EQ -> 0 -- added EQ case, which covers all data constructors of type Ordering
```

### 7.6 Higher-order functions
When we want to express a function argument within a function type, we must use parentheses to nest it, such as `flip`:
```haskell
flip :: (a -> b -> c) -> b -> a -> c
flip f x y = f y x
```

#### Exercises: Artful Dodgy
Given
```haskell
dodgy x y = x + y * 10
oneIsOne = dodgy 1
oneIsTwo = (flip dodgy) 2
```
give normalized values for the following

4. `dodgy 1 2` **= 21**
9. `oneIsTwo 2` **= 22**
10. `oneIsOne 3` **= 31**

### 7.7 Guards
Guard syntax allows us to write compact functions that allow for two or more possible outcomes depending on the truth of the conditions.
```haskell
myAbs :: Integer -> Integer
myAbs x
  | x < 0     = (-x)
  | otherwise = x
```

### 7.8 Function composition
```haskell
(.) :: (b -> c) -> (a -> b) -> a -> c
```
Just keep in mind **precedence** of function application (10/10) is greater than composition operator (9/10). Consequentially:
```haskell
-- works
negate . sum $ [1, 2, 3, 4, 5] -- -15

--works
(negate . sum) [1, 2, 3, 4, 5] -- -15

--TYPE ERROR
negate . sum [1, 2, 3, 4, 5]
negate . 15 -- composing (Num a => a -> a) with (Num a => a)
```
When composing more and more functions, the lack of parentheses is more and more useful:
```haskell
λ> take 5 . filter odd . enumFrom $ 3
[3,5,7,9,11]
```

### 7.9 Pointfree style
Pointfree refers to a style of composing functions without specifying their arguments. This let's us focus on *functions* rather than *data*.
```haskell
f xs = negate . sum $ xs
f = negate . sum

g z xs = foldr (+) z xs
g = foldr (+)

print a = putStrLn (show a)
print = putStrLn . show
```

### 7.11 Chapter Exercises
#### Multiple choice
##### 2. Two functions named `f` and `g` have types `Char -> String` and `String -> [String]` respectively. The composed function `g . f` has the type
 - [ ] `Char -> String`
 - [x] `Char -> [String]`
 - [ ] `[[String]]`
 - [ ] `Char -> String -> [String]`


##### 3. A function `f` has the type `Ord a => a -> a -> Bool` and we apply it to one numeric value. What is the type now?
 - [ ] `Ord a => a -> Bool`
 - [ ] `Num -> Num -> Bool`
 - [ ] `Ord a => a -> a -> Integer`
 - [x] `(Ord a, Num a) => a -> Bool`

##### 4. A function with the type `(a -> b) -> c`
 - [ ] requires values of three different types
 - [x] is a higher-order function
 - [ ] must take a tuple as its first argument
 - [ ] has its parameters in alphabetical order (the type variables in signature are **not parameters**)
