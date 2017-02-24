# Non-strictness

Technically, Haskell is "non-strict" and not "lazy".
In a nutshell, expressions are reduced when necessary,
and when the reduction (evaluation) begins, a **thunk** is created for each expression.
The thunk acts like a placeholder in the underlying graph of the program.
It can be evaluated when necessary, but if it's never needed,
it never gets reduced and the garbage collector sweeps it away.
Once it is evaluated, it is usually shared;
that is if `x` is needed twice and `x = 1 + 1`, the second time it does not need to be recomputed.
This fits with my overall intuition so far.

A truly **lazy** language memoizes the results of all functions it evaluates.
For larger programs, this results in unacceptably large amounts of memory.
Implementations of Haskell such as GHC are only obligated to be non-strict with respect to bottom.
Basically, any given implementation of non-strictness is acceptable
as long as it respects when it's supposed to return a value successfully or bottom out.

### 27.3 Outside in, inside out
Strict languages evaluate *inside out*, while non-strict languages evaluate *outside in*.
The order of evaluation can depend on which values are being forced.
We've seen this from playing with evaluations such as
```haskell
λ> const 'a' undefined
'a'

λ> snd (putStrLn "hello", putStrLn "world")
world
```

### 27.4 What does the other way look like?
And we can get away with defining arbitrary bindings to bottom:
```haskell
hypo :: IO ()
hypo = do
  let x :: Int
      x = undefined
  s <- getLine
  case s of
    "hi" -> print x
    _    -> putStrLn "hello"
```
For strict languages, this is a problem - a strict language will force the bottom before binding `x`.
In general, it evaluates each binding as it comes into scope, not when the binding is used.

The idea is that evaluation is **driven by demand**, not by *construction*.

#### Can we make Haskell strict?
Let's try to make Haskell treat `hypo` as a strict language would:
```haskell
hypo' :: IO ()
hypo' = do
  let x :: Integer
      x = undefined
  s <- getLine
  case x `seq` s of
    "hi" -> print x
    _    -> putStrLn "hello"
```
The `seq :: a -> b -> b` function magically forces evaluation of the first argument
whenever the second argument has to be evaluated.
Still, this isn't quite strict, since we ran `getLine` successfully.

#### `seq` and ye shall find
In old versions of Haskell, `seq` had type `seq :: Eval a => a -> b -> b`
and the `Eval` typeclass had a method for forcing evaluation.
```haskell
seq bottom b    = bottom
seq notBottom b = b
```
We need this evaluator to look `const`-ish because evaluation is **demand driven**,
so we create links between nodes in the graph of expressions
where forcing one expression will force yet another expression.
Of course, this does mean that we cannot actually guarantee evaluation, ever.
```haskell
undefined `seq` y
-- forcing y -> forces undefined

undefined `seq` y `seq` x
-- forcing x -> forces y -> forces undefined
```

#### `seq` and weak head normal form
`seq` evaluates your expression up to **weak head normal form**,
which basically means it stops at the first data constructor or lambda
(for more info head to *Parallel and Concurrent Programming in Haskell*).
```haskell
Prelude> let dc = (,) undefined undefined
Prelude> let noDc = undefined
Prelude> let lam = \_ -> undefined
Prelude> dc `seq` 1
1
Prelude> lam `seq` 1
1
Prelude> noDc `seq` 1
*** Exception: Prelude.undefined
```

#### Case matching also chains evaluation
This is pretty obvious; the values we pattern match on are forced
because Haskell doesn't know which data constructor is relevant until it is evaluated
to the depth required (which is the depth of the data constructors that we pattern match).
```haskell
f :: Either a -> Int
f (Left _)  = 1
f (Right _) = 2

λ> f undefined
*** Exception: Prelude.undefined

λ> f (Right undefined)
2
```

#### Core Dump
So far we've investigated strictness by injecting bottoms
and seeing whether or not they get evaluated.
We can also look at the underlying language that GHC Haskell gets simplified to:
```haskell
-- CoreDump.hs
module CoreDump where

discriminatory :: Bool -> Int
discriminatory b =
  let x = undefined
  case b of
    False -> 0
    True  -> 1

λ> :set -ddump-simpl
λ> :set -dsuppress-all 
λ> :l CoreDump.hs

discriminatory
discriminatory =
  \ b_a2ze ->
    case b_a2ze of _ {
      False -> I# 0#;
      True -> I# 1#
    }
```
We're looking for case expressions in GHC Core, since those must be evaluated.
Note that GHC Core knows we never use `x` so it drops it off.
Now let's see what `seq` does:
```haskell
discriminatory :: Bool -> Int
discriminatory b =
  let x = undefined
  in case x `seq` b of
    False -> 0
    True  -> 1

-- GHC Core
discriminatory =
\ b_a10D ->
    let {
      x_a10E
      x_a10E = undefined } in
    case
      case x_a10E of _ {
        __DEFAULT -> b_a10D
      } of _ {
        False -> I# 0;
        True -> I# 1
      }
```
Now there are *two* nested case expressions to force `x` before `b`.
Notice this is the same as the first except `b_a10D`
is replaced by `case x_a10E of _ { __DEFAULT -> b_a10D }`.

##### A core difference
In Haskell, only pattern matching on a case is strict up to WHNF
while in Core, cases are always strict regardless of pattern matching:
```haskell
-- Haskell
-- does not bottom out
-- Core actually drops this entire case expression during compilation
case undefined of { _ -> False }

-- Core
-- even with just one case without matching on anything, this evaluates bottom
case undefined of { DEFAULT -> False }
```
Keep in mind Core and Haskell are not the same language;
however Core is convenient for seeing if two expressions are equivalent.

#### Exercises: Evaluate 
What do these evaluate to?

1. `const 1 undefined`

  ```
  1
  ```
2. `const undefined 1`

  ```
  bottom
  ```
3. `flip const undefined 1`

  ```
  1
  ```
4. `flip const 1 undefined`

  ```
  bottom
  ```
5. `const undefined undefined`

  ```
  bottom
  ```
6. `foldr const 'z' ['a'..'e']`

  ```
  const 'a' (foldr const 'z' ['b'..'e'])
  const 'a' (const 'b' (foldr const 'z' ['c'..'e']))
  ...
  'a'
  ```
7. `foldr (flip const) 'z' ['a'..'e']`
  ```
  let f = flip const
  f 'a' (foldr f 'z' ['b'..'e'])
  f 'a' (f 'b' (foldr f 'z' ['c'..'e']))
  ...
  f 'a' (.. (.. f 'e' 'z'))
  'z'
  ```

### 27.5 Call by name, call by need
