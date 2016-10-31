# 3.2 Instances
This section is a showcase of some common functor instances, nothing new after going
through *Learn You a Haskell*. I am including the "proof" exercises 4 and 5 because they
are better suited to markdown.

#### 4. Give an example of a type of kind * -> * which cannot be made an instance of Functor (without using undefined).
```haskell
data Jumble a = Jumble (a -> String)
```
Jumble cannot be made an instance of Functor because `fmap :: (a -> b) -> f a -> f b`,
and in this case, notice that `f a === Jumble (a -> String)`. So what would we do with
the inital argument that is a function of type `(a -> b)`? We need to end up with a type
of `Jumble (b -> String)`, in other words a function that accepts type `b`. There's no
way for us to do this because we aren't guaranteed an inverse of any of these functions.
Note this is like the opposite of `((->) b)`, where the type parameter on which Functor
operates is the *second* parameter.

Yeah I know, I'm not satisfied with that explanation either. I found other people who go
into more detail ([[1](http://stackoverflow.com/questions/26985562/make-data-type-of-kind-thats-not-a-functor/26986211#26986211)] [[2](https://michaelochurch.wordpress.com/2016/01/01/insights-into-why-a-contravariant-type-cant-be-a-haskell-functor/)]). At least I had the right idea with a function of type `a -> b`.


#### 5. Is this statement true or false: The composition of two Functors is also a Functor
I believe this is true.

**Proof**: Consider types `A` and `B` which are each of kind `* -> *`. Without loss of generality,
let us consider the composed type `C = B A`, which is also of kind `* -> *`. Since `A` and `B`
are each Functors, we know that for any `x`,`y` of indeterminate type,
```haskell
fmap f (B x) = B (f x)
fmap f (A y) = A (f y)
```
Therefore we can define `fmap` for type `C` as
```haskell
fmap f (C z) =
fmap f (B (A z)) = let (A y) = fmap f (A z)
                   in (A y) <$ (B z) -- does second parameter matter?
``` 
Notice we only need to `fmap f` once, to transform `(a -> b)`. The second usage of the Functor class
is just `<$` which is `fmap . const`, and is just used to take the already fmapped value (in context of `A`)
and minimally put it into the context of `B`.
Note: This proof sucks.

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
