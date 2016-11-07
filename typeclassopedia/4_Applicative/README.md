# 4.1 Definition
Defined in `Control.Applicative`:
```haskell
class Functor f => Applicative f where
  pure  :: a -> f a
  infixl 4 <*>, *>, <*
  (<*>) :: f (a -> b) -> f a -> f b
 
  (*>) :: f a -> f b -> f b
  a1 *> a2 = (id <$ a1) <*> a2
 
  (<*) :: f a -> f b -> f a
  (<*) = liftA2 const
```
1. `<*>` is similar to `fmap` for applying functions to values with context, however this is used when the function we are applying is *also* within context. `<*>` is pronounced as *apply*, *app*, or *splat*.
2. `pure` embeds values in a default *effect free* context.
3. `*>` sequences the effects of two computations, but discards the result of the first
4. `<*` sequences the effects of two computations, but discards the result of the second

 **Note** similar to `<$` and `$>`, the array points to the value being "kept"

Things to note here:
- `<*>` similarity to `fmap`, except applied function `a -> b` is already wrapped in context `f`.
- `<*>` similarity to `$`, except all types wrapped in context `f`. (Hence the applicator name.)

# 4.2 Laws
The traditional laws around `Applicative` instances are about ensuring that `pure` is pure.

| Rule                                          | Name         |
| --------------------------------------------- | ------------ |
| `pure id <*> v = v`                           | Identity     |
| `pure f <*> pure x = pure (f x)`              | Homomorphism |
| `u <*> pure y = pure ($ y) <*> u`             | Interchange  |
| `u <*> (v <*> w) = pure (.) <*> u <*> v <*> w`| Composition  |

1. **Identity**: A lifted `id` function acts as `id` for values in context
2. **Homomorphism**: Intuitively, applying a non-effectful function to a non-effectful argument in an effectful context is the same as just applying the function to the argument and then injecting the result into the context with pure.
3. **Interchange**: Intuitively, this says that when evaluating the application of an effectful function to a pure argument, the order in which we evaluate the function and its argument doesn't matter.
4. **Composition**: Associativity of lifted compositions.

Notice how each law translates an expression into a canonical form with only a single use of `pure` at the beginning, and only left-nested occurences of `<*>`.

Another law relates back to `Functor`:

1. `fmap g x = pure g <*> x`

This better hold true, otherwise bringing `g` into context via `pure g` would be a sham.

#### 1. (Tricky) One might imagine a variant of the interchange law that says something about applying a pure function to an effectful argument. Using the above laws, prove that
`pure f <*> x = pure (flip ($)) <*> x <*> pure f`

**Proof**:
```haskell
-- Let u = pure f and x = pure y
pure (flip ($)) <*> x <*> pure f  =  pure (flip ($)) <*> pure y <*> pure f  -- by definition
                                  =  pure (flip ($) y) <*> pure f           -- by Homomorphism
                                  =  pure ((flip ($)) y f)                  -- by Homomorphism
                                  =  pure (f y)                             -- by definition of flip,$
                                  =  pure f <*> pure y                      -- by Homomorphism
                                  =  pure f <*> x                           -- by definition
```
Note: This assumes existence of inverse of `pure`, that is, retrieving a direct value from a context.

# 4.3 Instances
