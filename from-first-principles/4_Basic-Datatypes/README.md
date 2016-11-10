# Because Pigs Can't Fly

### 4.3 Given the following datatype, answer the following questions:

```haskell
data Mood = Blah | Woot deriving Show
```

1. What is the type constructor, or name of this type?

  **Mood**

2. If the function requires a `Mood` value, what are the values you could possibly use there?

  **Blah, Woot**

3. We are trying to write a function `changeMood` to change Chris’s mood instantaneously. It should act like `not` in that, given one value, it returns the other value of the same type. So far, we’ve written a type signature `changeMood :: Mood -> Woot`. What’s wrong with that?

  **Woot is not a type, just a data constructor. Furthermore only returning Woot wouldn't solve returning opposites.**

4. Now we want to write the function that changes his mood. Given an input mood, it gives us the other one. Fix any mistakes and complete the function:

  ```haskell
  changeMood Mood = Woot
  changeMood
  _ = Blah
  ```
  **Corrected:**
  ```haskell
  changeMood :: Mood -> Mood
  changeMood Blah = Woot
  changeMood Woot = Blah
  ```

### 4.7 Tuples

The difference between `data Bool = True | False` and `data (,) a b = (,) a b` is that the former is a *sum type* representing logical disjunction, while the latter is *product type* representing logical conjunction. In other words, that pipe means that `Bool` is *either* `True` or `False`, while `(a,b)` is both `a` *and* `b`.

With the following type signatures:
```haskell
fst :: (a, b) -> a
snd :: (a, b) -> b
```
we can argue that there's really no way these functions could do something other than simply return the first or second value.

### 4.9 Exercises
Assume the following functions are defined:
```haskell
awesome = ["Papuchon", "curry", ":)"]
alsoAwesome = ["Quake", "The Simons"]
allAwesome = [awesome, alsoAwesome]
```

##### 3. Given what we know about numeric types and the type signature of length, look at these two expressions. One works and one returns an error. Determine which will return an error and why.
```haskell
Prelude> 6 / 3
-- and
Prelude> 6 / length [1, 2, 3]
```
The second returns an error because `length` specifically returns type `Int`, which doesn't implement `Fractional` whereas the compiler can infer that `3` should be a type implementing `Fractional` in the expression `6 / 3`.

##### 4. How can you fix the broken code from the preceding exercise using a different division function/operator?
```haskell
Prelude> 6 `div` length [1, 2, 3]
```

##### 5. What is the type of the expression 2 + 3 == 5? What would we expect as a result?
Type: **Bool** and value **True**

##### 7. Below are some bits of code. Which will work? Why or why not? If they will work, what value would these reduce to?
```haskell
Prelude> length allAwesome == 2 -- True
Prelude> length [1, 'a', 3, 'b'] -- FAILS: list contains values of varying type
Prelude> length allAwesome + length awesome -- 5
Prelude> (8 == 8) && ('b' < 'a') -- False
Prelude> (8 == 8) && 9 -- FAILS: 9 is not of type Bool and (&&) :: Bool -> Bool -> Bool
```

##### 8. Write a function that tells you whether or not a given String (or list) is a palindrome. Here you’ll want to use a function called ’reverse,’ a predefined function that does just what it sounds like.
```haskell
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == reverse x
```

##### 9. Write a function to return the absolute value of a number using if-then-else
```haskell
myAbs :: Integer -> Integer myAbs = undefined
myAbs x = if (x >= 0) then x else -x
```

##### 10. Fill in the definition of the following function, using fst and snd:
```haskell
f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f x y = ((snd x, snd y), (fst x, fst y))
```

##### Match the functions to their types
1. Which of the following types is the type of `show`?
 - [ ] `show a => a -> String`
 - [ ] `Show a -> a -> String`
 - [x] `Show a => a -> String`

2. Which of the following types is the type of `(==)`?
 - [ ] a -> a -> Bool`
 - [x] Eq a => a -> a -> Bool`
 - [ ] Eq a -> a -> a -> Bool`
 - [ ] Eq a => A -> Bool`

3. Which of the following types is the type of `fst`?
 - [x] `(a, b) -> a`
 - [ ] `b -> a`
 - [ ] `(a, b) -> b`

4. Which of the following types is the type of `(+)`?
 - [ ] `(+) :: Num a -> a -> a -> Bool`
 - [ ] `(+) :: Num a => a -> a -> Bool`
 - [ ] `(+) :: num a => a -> a -> a`
 - [x] `(+) :: Num a => a -> a -> a`
 - [ ] `(+) :: a -> a -> a`

### 4.10 Definitions

*Arity* is the number of arguments a function accepts

*Polymorphism* in Haskell means being able to write code in terms of values which may be one of several, or any, type.
Polymorphism in Haskell is either parametric or constrained.
The identity function, `id`, is an example of a parametrically polymorphic function:
```haskell
id :: a -> a
id x = x
```
Here `id` works for a value of any type because it doesn’t use any information specific to a given type or set of types.
Whereas,
```haskell
isEqual :: Eq a => a -> a -> Bool
isEqual x y = x == y
```
is polymorphic, but constrained or bounded to the set of types which have an instance of the `Eq` typeclass.
(To be discussed more in later chapters.)
