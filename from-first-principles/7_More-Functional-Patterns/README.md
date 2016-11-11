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
