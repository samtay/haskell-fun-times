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

### 27.7 Thunk Life
A **thunk** is used to reference suspended computations
that may or may not be performed/computed at a later point in the program.

#### Not all values get thunked
We'll use GHCi `sprint`, which shows what has been evaluated.
Unevaluated values are represented as `_`.
It can be quirky, but this chapter explains the quirks.
```haskell
λ> let l = [1,2,3] :: [Integer]
λ> :sprint l
l = [1,2,3]
```
The list is already fully evaluated! This is due to **opportunistic strictness**.
GHC will not thunk values which are merely data constructors, since data constructors are constant,
so this is better for optimization.
Above, the data constructors are `:`, `[]`, and each integer `1`, `2`, `3`.

Since all of those are immediately fully applied, the list is fully evaluated.
However, remember that data constructors are evaluated to *weak head normal form*, so
the evaluation of data constructors does not continue past WHNF:
```haskell
λ> let l = [1,2,3,id 4] :: [Integer]
λ> :sprint l
l = [1,2,3,_]
```

Another thing to keep in mind is that above, `l` gets evaluated in part because
we already know which data constructors to use.
If we don't specify the type, there will be an implicit function `Num a -> a`
awaiting application and thus the list will get thunked:
```haskell
λ> let l = [1,2,3]
λ> :sprint l
l = _
```

Lastly, consider what happens when we concat this list with another:
```haskell
λ> let l = [1,2,3]
λ> let l' = l ++ undefined
λ> :sprint l'
l' = _
```
The whole expression gets thunked because the outermost term is now a *function* `(++)`
instead of a data constructor like `(:)`.

### 27.8 Sharing is caring
Sharing refers to sharing the results of a computation with all references to that name
without re-evaluating it.
This is important because memory is finite.
As mentioned previously, GHC oscillates between call-by-need and call-by-name;
that is, it doesn't share all the time, but shares when it can
and when it thinks it will produce faster code.

#### Using trace to observe sharing
The *base* library has a `Debug.Trace` module that's useful for investigating sharing.
Note: it cheats the type system and uses `putStrLn` outside of `IO`.
It should **never** be used in real code, just for experimentation.
```haskell
λ> import Debug.Trace
λ> a = trace "a" 1
λ> b = trace "b" 2
λ> a + b
b
a
3
```
Wow. Evaluation of the arguments to addition `(+)` is not guaranteed.

#### What promotes sharing
**Names** promote sharing when possible.
Even if two names are bound to the same value they have to be evaluated separately.
So if we only want to evaluate `1 :: Int` once, we can name it a single variable:
```haskell
-- two separate "names"
λ> x = trace "x" (1 :: Int)
λ> y = trace "y" (1 :: Int)
λ> x + y
x
y
2

-- just one "name"
λ> x = trace "x" (1 :: Int)
λ> x
x
1
λ> x
1
```
Notice the last three times we used `x`, its value was already held in memory
and didn't need to be evaluated again.

There are some counterintuitive scenarios though.
One such example is construction of strings: `['a']` versus `"a"`:
```haskell
λ> let a = Just ['a']
λ> :sprint a
a = Just "a"

λ> let a = Just "a"
λ> :sprint a
a = Just _
```
The two examples above have the same value but are created using different data constructors,
and the GHC optimization analysis is limited to data constructors, not computation.
Of course, this means that the syntactic sugar for strings is represented differently in GHC Core:
```haskell
-- Just ['a']
((Just (: (C# 'a'#) [])) `cast` ...)

-- Just "a"
((Just (unpackCString# "a"#)) `cast` ...)
```
GHC does this purposefully for more optimization opportunities,
such as converting string literals to ByteString or Text values.

#### What prevents sharing
**Inlining expressions** where they get used prevents sharing,
as it creates independent thunks that get computed separately.
```haskell
λ> let f _ = trace "f" 1
λ> f 'a'
f
1
λ> f 'a'
f
1
```

When we assign a name to the value of `(2 + 2)`, it is shared:
```haskell
Prelude> let a :: Int; a = trace "a" 2 + 2
Prelude> let b = (a + a)
Prelude> b
a
8
Prelude> b
8
```
But inline expressions equivalent to `a` won't get shared without being bound to the same name:
```haskell
λ> let c :: Int; c = (trace "a" 2 + 2) + (trace "a" 2 + 2)
Prelude Debug.Trace
λ> c
a
a
8
Prelude Debug.Trace
λ> c
8
```

Note that a **function with explicit arguments** is not shared.
As noted earlier, Haskell is *not fully lazy*, so it is not required to remember
the result of function application for a given set of arguments:
```haskell
λ> let f = trace "f" const $ 1
λ> f 'a'
f
1
λ> f 'b'
f
1
```
However this is for **explicit arguments**.
Eta reduction to pointfree code (dropping arguments) can change sharing properties
(explained in next chapter).

**Typeclass constraints** also prevent sharing.
If we omit the concrete type hints from earlier examples, things get re-evaluated:
```haskell
λ> let a = trace "a" 2 + 2
λ> let b = (a + a)
λ> b
a
a
8
λ> b
a
a
8
```
This is because typeclass constraints are a function in Core,
awaiting application to become concrete types.

Implicit parameters have a similar effect on sharing to that of typeclass constraints
because both decay into function arguments when compiled.
```haskell
λ> let add :: (?x :: Int) => Int; add = trace "add" 1 + ?x
λ> let ?x = 1 in add
add
2
λ> let ?x = 1 in add
add
2
```

#### Why polymorphic values never seem to get forced
First let's observe this in action. Leaving the `1` polymorphic:
```haskell
λ> blah = Just (trace "evaled 1" 1)
λ> fmap (+1) blah
Just evaled 1
2
λ> fmap (+1) blah
Just evaled 1
2
```
the `blah` is evaluated twice, and thus the `fmap` expression is not shared.
If we make `1` concrete:
```haskell
λ> blah = Just (trace "evaled 1" (1 :: Int))
λ> fmap (+1) blah
Just evaled 1
2
λ> fmap (+1) blah
Just 2
```
then the `fmap` expression is shared.

This is because in GHC Core, polymorphic values that have a simple typeclass constraint
such as `Num a => a` are represented as lambdas awaiting a typeclass instance argument.
These types can become concrete through assignment or type defaulting,
but if they are not concrete, the lambda will need to be re-evaluated every time
since unapplied functions are not shareable values.
Hence, polymorphic expressions cannot be shared.

#### Preventing sharing on purpose
We might want to *prevent* sharing to prevent a large datum in memory
that was calculated to provide a much smaller answer.
It turns out we can do this by adding named arguments, as opposed to pointfree expressions.
Observe:
```haskell
λ> f x = (x 2) + (x 10)

-- not shared
λ> g = \_ -> trace "hi" 2
λ> f g
hi
hi
4

-- shared
λ> g = const (trace "hi" 2)
λ> f g
hi
4
```

#### Forcing sharing
This one is easy - just give your expression a name. We can use `let`:
```haskell
-- not shared
λ> (trace "eval 1 plus 1" (1 + 1) :: Int) + (trace "eval 1 plus 1" (1 + 1) :: Int)
eval 1 plus 1
eval 1 plus 1
4

-- shared
λ> let x = (trace "eval 1 plus 1" (1 + 1) :: Int) in x + x
eval 1 plus 1
4
```

This is a very important concept to remember for production applications
and any long running process.
For example, look at the `forever` definition:
```haskell
forever :: (Monad m) => m a -> m b
forever a = let b = a >> b in b
```
The let expression is used because we need to force sharing here.
Without it we'd be leaking memory with each monadic action,
but with it GHC overwrites the thunk of each monadic action.

### 27.9 Refutable and irrefutable patterns
When pattern matching, an **irrefutable** pattern is one that *always matches*,
and a **refutable** pattern is one with potential failures.
```haskell
refutable :: Bool -> Bool
refutable True = False
refutable False = True

irrefutable :: Bool -> Bool
irrefutable x = not x

oneOfEach :: Bool -> Bool
oneOfEach True = False
oneOfEach _ = True
```

#### Lazy patterns
Lazy patterns are irrefutable. We can make a pattern match lazy with `~`:
```haskell
lazyPattern :: (a, b) -> String
lazyPattern ~(a,b) = const "Cousin It" a
```
but since it makes an irrefutable match, we can't discriminate against sum types.
So it's really only useful for deconstructing product types that might not get used:
```haskell
λ> lazyPattern undefined
"Cousin It"
```
The default, without `~`, just goes ahead and forces it
for more predictable memory usage and performance.

### 27.10 Bang-patterns
Using the pragma *BangPatterns* we can force evaluation of arguments:
```haskell
{-# LANGUAGE BangPatterns #-}
module Bang where

doesntEval :: Bool -> Int
doesntEval b = 1

manualSeq :: Bool -> Int
manualSeq b = b `seq` 1

bang :: Bool -> Int
bang !b = 1
```
Note that `manualSeq` and `bang` compile to the *same* function in GHC Core
and are thus completely equivalent.

#### Bang patterns in data
Just as we can force evaluation of arguments to functions,
we can force evaluation of arguments to data constructors:
```haskell
data Foo = Foo Int !Int

first (Foo x _) = x
second (Foo _ y) = y

λ> second (Foo undefined 1)
1
λ> first (Foo 1 undefined)
*** Exception: Prelude.undefined
```

We want this capability because **sometimes  it's cheaper to just compute something
rather than construct a thunk and evaluate later**.
This is common in numerics, as it's very cheap to individually construct values
of types such as `Int` and `Double`.

Again, as mentioned way back, the rule of thumb is
**lazy in the spine, strict in the leaves**.
For example, a strict large list (spine) of 1,000,000 lazy tiny values (integers)
made into less-tiny thunks can result in very excessive memory usage
when we could've just computed them on the spot.

### 27.11 Strict and StrictData
In GHC 8.0 and newer, there are *Strict* and *StrictData* pragmas
that help avoid slamming code with `seq` and bang patterns without
adding any new semantics.
It doesn't make lazy data structures defined elsewhere behave differently,
only the ones defined within the module with the pragma.
We can still have irrefutable patterns be lazy by using the tilde:
```haskell
{-# LANGUAGE Strict #-}
module StrictTest where

-- equivalent to
-- willForce !x = 1
willForce x = 1

willNotForce ~x = 1
```

### 27.13 Chapter Exercises

#### What will `:sprint` output?
1. `let x = 1`

  ```haskell
  x = _
  ```

2. `let x = ['1']`

  ```haskell
  x = "1"
  ```

3. `let x = [1]`

  ```haskell
  x = _
  ```

4. `let x = 1 :: Int`

  ```haskell
  x = 1
  ```

5. `let f = \x -> x` and `let x = f 1`

  ```haskell
  f = _
  x = _
  ```

6. `let f :: Int -> Int; f = \x -> x` and `let x = f 1`

  ```haskell
  f = _
  x = _
  ```

#### Will printing this expression result in bottom?
1. `snd (undefined, 1)

  **No**

2. `let x = undefined` and `let y = x `seq` 1 in snd (x,y)

  **Yes**

3. length $ [1..5] ++ undefined

  **Yes**

4. length $ [1..5] ++ [undefined]

  **No**

5. const 1 undefined

  **No**

6. const 1 (undefined `seq` 1)

  **No**

7. const undefined 1

  **Yes**

#### Make the expression bottom
Using only bang patterns or `seq`, make the below bottom out when executed:
```haskell
-- Initial code
x = undefined
y = "blah"
main = do
  print (snd (x, y))

-- Bottoming code
x = undefined
y = "blah"
main = do
  print (snd (x, x `seq` y))
```
