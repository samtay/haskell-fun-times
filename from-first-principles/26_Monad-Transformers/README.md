# Monad transformers
Chapter [25](../25_Composing-Types) demonstrated the need for monad transformers
and what they accomplish.
This chapter is a deeper dive to get more comfortable with them in practice.

### 26.2 MaybeT
Just as the Maybe monad is extremely useful,
its transformer variant MaybeT is an important transformer.
```haskell
newtype MaybeT m a =
  MaybeT { runMaybeT :: m (Maybe a) }
```
Notice the similarity in the structures of the MaybeT and Compose types;
that structure leads to reusing the same pattern for functor & applicative instances:
```haskell
instance (Functor m) => Functor (MaybeT m) where
  fmap f (MaybeT ma) =
    MaybeT $ (fmap . fmap) f ma

instance (Applicative m) => Applicative (MaybeT m) where
  pure x = MaybeT . pure . pure
  (MaybeT mMab) <*> (MaybeT mMa) =
    MaybeT $ liftA2 (<*>) mMab mMa
    -- or
    MaybeT $ (<*>) <$> mMab <*> mMa
```
Again, we only need to do something different once we get to the monad instances,
since functors & applicatives are closed under composition in general.

#### MaybeT Monad instance
Nothing too surprising here:
```haskell
instance (Monad m) => Monad (MaybeT m) where
  return = pure

  (>>=) :: MaybeT m a
        -> (a -> MaybeT m b)
        -> MaybeT m b
  (MaybeT mMa) >>= aMmb =
    MaybeT $ do
      -- v :: Maybe a
      v <- mMa
      case v of
        Nothing -> return Nothing
        Just y  -> runMaybeT $ aMmb y
```

### 26.3 EitherT
This is the newtype for the Either transformer:
```haskell
newtype EitherT e m a =
  EitherT { runEitherT :: m (EitherT e a) }
```

#### Exercises: EitherT
1. Write the Functor instance for EitherT.

  ```haskell
  instance (Functor f) =>
    Functor (EitherT e f) where
      fmap f (EitherT ma) = EitherT $ (fmap . fmap) f ma
  ```

2. Write the Applicative instance for EitherT.

  ```haskell
  instance (Applicative f) =>
    Applicative (EitherT e f) where
      pure = EitherT . pure . pure

      (EitherT mf) <*> (EitherT ma) =
        EitherT $ (<*>) <$> mf <*> ma
  ```

3. Write the Monad instance for EitherT.

  ```haskell
  instance (Monad m) =>
    Monad (EitherT e m) where
      return = pure

      (EitherT ma) >>= f = EitherT $ do
        -- a :: Either e a
        a <- ma
        case a of
          Left e  -> return $ Left e
          Right x -> runEitherT $ f x
  ```

4. Write the `swapEitherT` helper function for EitherT.

  ```haskell
  swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
  swapEitherT (EitherT ma) = EitherT $ swapEither <$> ma
  ```

5. Write the transformer variant of the `either` catamorphism.

  ```haskell
  eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
  eitherT f g (EitherT ma) = ma >>= either f g
  ```

### 26.4 ReaderT
ReaderT is one of the most common transformers in conventional Haskell.
It's just like Reader, but generating additional structure in the return type:
```haskell
newtype ReaderT r m a =
  ReaderT { runReaderT :: r -> m a }
```
I decided to write the instances myself,
and they look a bit different than the book's,
but they type check just fine:
```haskell
instance (Functor m) =>
  Functor (ReaderT r m) where
    fmap f (ReaderT rma) = ReaderT $ fmap f . rma

instance (Applicative f) =>
  Applicative (ReaderT e f) where
    pure x = ReaderT $ const (pure x)

    (<*>) :: ReaderT r f (a -> b)
          -> ReaderT r f a
          -> ReaderT r f b
    (ReaderT mf) <*> (ReaderT ma) =
      ReaderT $ \r -> (mf r) <*> (ma r)

instance (Monad m) => Monad (ReaderT r m) where
  return = pure

  (>>=) :: ReaderT r m a
        -> (a -> ReaderT r m b)
        -> ReaderT r m b
  (ReaderT ma) >>= f =
    ReaderT $ \r -> do
    a <- ma r
    runReaderT (f a) r
```

### 26.5 StateT
StateT is State with additional monadic structure around the function result, just like Reader and ReaderT.
```haskell
newtype StateT s m a =
  StateT { runStateT :: s -> m (a, s) }
```

#### Exercises: StateT
We're implementing the strict variant (as opposed to lazy), which is the "obvious" implementation:
```haskell
instance (Functor m) =>
  Functor (StateT s m) where
    fmap f (StateT smf) = StateT $ \s ->
      fmap (\(x, s1) -> (f x, s1)) (smf s)

instance (Monad m) =>
  Applicative (StateT s m) where
    pure x = StateT $ \s -> pure (x, s)

    (<*>) :: StateT s m (a -> b)
          -> StateT s m a
          -> StateT s m b
    (StateT smf) <*> (StateT sma) = StateT $ \s1 -> do
      (ab, s2) <- smf s1
      (a, s3)  <- sma s2
      return (ab a, s3)

instance (Monad m) => Monad (StateT s m) where
  return = pure

  (>>=) :: StateT s m a
        -> (a -> StateT s m b)
        -> StateT s m b
  (StateT sma) >>= f = StateT $ \s1 -> do
    (a, s2) <- sma s1
    (runStateT (f a)) s2
```
Notice we need a monadic constraint for the applicative instance;
this is needed to feed the successive states through each computation,
as opposed to just feeding the initial state through each computation.

### 26.6 Types you probably don't want to use
Not every type will necessarily be performant or make sense, such as ListT and Writer/WriterT.

#### Why not use Writer or WriterT?
First of all, State can do what Writer can do plus more.
In addition, it is commonly too lazy or too strict for a given problem,
which results in too much memory usage.
Writer can accumulate unevaluated thunks, causing memory leaks.
For example, it is inappropriate for logging in long-running processes,
since the logged values can't be retrieved until the computation is complete.

#### The ListT you want isn't made from the List type
The obvious implementation of ListT is generally not recommended because:
1. The obvious obvious attempt isn't associative.
2. It's not very fast.
3. Streaming libraries like [pipes](http://hackage.haskell.org/package/pipes) and [conduit](http://hackage.haskell.org/package/conduit) do it better for most cases.

Lists in Haskell are as much a control structure as they are a data structure,
so streaming libraries generally suffice when we need transformers.

### 26.7 Recovering an ordinary type from a transformer
