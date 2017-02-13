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
