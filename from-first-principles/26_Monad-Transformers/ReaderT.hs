{-# LANGUAGE InstanceSigs #-}
module ReaderT where

newtype ReaderT r m a =
  ReaderT { runReaderT :: r -> m a }

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
