# Applicative
So far we've seen Monoids, a way of joining two values of the same type together, and Functors, a way to apply functions over some structure. Applicativce can be thought of as a *monoidal functor*.
```haskell
class Functor f => Applicative f where
  pure  :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
```
The `pure` function embeds something into the (functorial, applicative) structure. This will be a *bare minimum* amount of structure, or structural *identity*. The `<*>` infix operator is called 'apply' or 'ap', and is just like `fmap` except the function argument is also in the structure:
```haskell
(<$>) :: Functor f =>       (a -> b) -> f a -> f b
(<*>) :: Applicative f => f (a -> b) -> f a -> f b
```

Along with these core functions, the Control.Applicative library provides
```haskell
liftA :: Applicative f =>
     (a -> b)
  -> f a
  -> f b
liftA2 :: Applicative f =>
     (a -> b -> c)
  -> f a
  -> f b
  -> f c
liftA3 :: Applicative f =>
     (a -> b -> c -> d)
  -> f a
  -> f b
  -> f c
  -> f d
```

### 17.3 Functor vs. Applicative
The difference between `<$>` and `<*>` seems small, but has profound impacts.
First of all note that even without the typeclass constraint, any Applicative could be made into a Functor:
```haskell
fmap f x = pure f <*> x
```

### 17.4 Applicative functors are monoidal functors
Noticing these similarities:
```haskell
($)   ::   (a -> b) ->   a ->   b
(<$>) ::   (a -> b) -> f a -> f b
(<*>) :: f (a -> b) -> f a -> f b
--       f          -> f   -> f
--         (a -> b) ->   a ->   b
```
we can see that `<*>` is just like function application `<$>` but the structure must be maintained throughout. This is where the monoidal aspect comes in - seeing the structure `f` on its own is reminiscent of `mappend :: Monoid a => a -> a -> a`.

To see this in action, let's look at the tuple:
```haskell
>>> fmap (+1) ("blah", 0)
("blah", 1)

>>> ("Woo", (+1)) <*> (" Hoo!", 0)
("Woo Hoo!", 1)

>>> :info (,)
instance Monoid a => Applicative ((,) a)
```

### Applicative in use
