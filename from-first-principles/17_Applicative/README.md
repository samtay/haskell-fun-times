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

See instances of Identity and Constant in [IdentityConstant.hs](./IdentityConstant.hs).

### Fixer Upper

##### `const <$> Just "Hello" <*> "World"`
```haskell
λ> const <$> Just "Hello" <*> pure "World"
Just "Hello"
```

##### `(,,,) Just 90 <*> Just 10 Just "Tierness" [1,2,3]
```haskell
λ> (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1,2,3]
Just (90,10,"Tierness",[1,2,3])
-- or
λ> (,,) <$> (liftA2 (+) (Just 90) (Just 10)) <*> Just "Tierness" <*> pure [1,2,3]
Just (100,"Tierness",[1,2,3])
```

### 17.6 Applicative Laws

1. **Identity**

  ```haskell
  pure id <*> v = v
  -- Putting 'id' into an applicative structure f via 'pure'
  -- should not produce side effects: f id <*> f x = f x
  ```
2. **Composition**

  ```haskell
  pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
  -- Putting the act of composition into an applicative structure
  -- should not produce side effects
  ```
3. **Homomorphism**

  ```haskell
  pure f <*> pure x = pure (f x)
  -- Basically assuring that 'pure' is homomorphic, preserving
  -- the algebraic isomorphism between categories,
  -- such as the normal space and the applicative 'f' space
  ```
4. **Interchange**

  ```haskell
  u <*> pure y = pure ($ y) <*> u
  -- Putting the act of function application into an applicative structure
  -- shuld not produce side effects
  ```

### 17.7 You knew this was coming
Of course, [quickchecking applicative laws](./QuickChecking.hs).

### 17.8 ZipList Monoid
The default list monoid in Prelude is concatenation, but if we constrain the type of the contents to monoids, here is another instance:
```haskell
[x1,x2,x3] <> [y1,y2,y3]
 = [x1 <> y1, x2 <> y2, x3 <> y3]
```
See [ZipList.hs](./ZipList.hs) for `List` and `ZipList` instances, and some quickchecking on them.
