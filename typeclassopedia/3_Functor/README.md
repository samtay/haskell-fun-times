# 3.3 Laws

While not guaranteed by Haskell itself, functors should satisfy these laws:

1. `fmap id = id`
2. `fmag (g . h) = (fmap g) . (fmap h)`

This ensures that `fmap g` changes a value without altering its context
These laws make `f` and `fmap` together an endofunctor on Hask, the category
of Haskell types (ignoring _|_) [ref](https://en.wikibooks.org/wiki/Haskell/Category_theory).

**Theorem 3.3.1**: A given type has at most *one* valid instance of Functor.

**Theorem 3.3.2**: The first law implies the second law.

I'm skeptical of **3.3.1**. The source linked in typeclassopedia goes no
where relevant, and I believe a counterexample exists for the type
`Pair a` defined in section [3.2](./3.2_Instances.hs). Consider
```haskell
-- my original solution
instance Functor Pair where
  fmap g (Pair x y) = Pair (g x) (g y)

-- alternative solution
instance Functor Pair where
  fmap g (Pair x y) = Pair x (g y)
```
What's wrong with us just ignoring the first element, like we do with `((,) e)`
and `Either e`?
```haskell
fmap id (Pair x y) = Pair x (id y) = Pair x y
id (Pair x y)                      = Pair x y
```
And there is no usage of `seq/undefined`... Sooo.. yeah. I don't believe **3.3.1**
is true but would love for someone to prove me wrong.

**ADDENDUM** I have proved myself wrong. The alternative instance defined above
is invalid and doesn't even pass type safety, because `g :: (a -> b)` and after
`fmap g` we would have a `Pair x y` value where `x :: a` and `y :: b`, conflicting
with the type definition `data Pair a = Pair a a`. Haskell +1, Sam -1.
