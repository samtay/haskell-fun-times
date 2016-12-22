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

### 17.5 Applicative in use
Taking a look at a list example:
```haskell
[(+1), (*2)] <*> [2, 4]
  = [ (+1) 2 , (+1) 4 , (*2) 2 , (*2) 4 ]
  = [3, 5, 4, 8]
```
The fact that it doesn't return two lists or a nested list or some other way in which the list structure would be preserved is the *monoidal* part. The fact that the functions are not simply concatenated with the values is the *function application* part.

Another example:
```haskell
Prelude> (,) <$> [1, 2] <*> [3, 4]
[(1,3),(1,4),(2,3),(2,4)]

-- or
Prelude> liftA2 (,) [1, 2] <*> [3, 4]
[(1,3),(1,4),(2,3),(2,4)]
```

Now, demonstrating utility:
```haskell
Prelude> fmap capitalize $ lookup 3 [(3, "hello")]
Prelude> capitalize <$> lookup 3 [(3, "hello")]
Just "Hello"

Prelude> let ls = [(3, "hello"), (4, "world)]
Prelude> (++) <$> lookup 3 ls <*> lookup 4 ls
Just "hello world"

Prelude> (++) <$> getLine <*> getLine
"hello "
"world"
"hello world"

Prelude> fmap length $ (++) <$> getLine <*> getLine
"hello "
"world"
11
```

#### Identity
Finally, we see a real use case for the `Identity` type we've been kicking around.
By introducing this "structure" on top of another structure, we can lift functions to map over the Identity instead of an underlying structure.
For instance, if we want to avoid messing with these lists, but still lift `const`:
```haskell
>>> const <$> [1, 2, 3] <*> [9, 9, 9]
[1,1,1,2,2,2,3,3,3]
>>> const <$> Identity [1, 2, 3] <*> Identity [9, 9, 9]
Identity [1,2,3]
```
In the first example, we have the list monoidal effect of `<*>`.
In the second, we limit the monoidal affects by having them applied to the wrapper structure `Identity`, where the "effects" are minimal.
