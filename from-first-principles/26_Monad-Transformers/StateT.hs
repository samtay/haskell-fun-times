{-# LANGUAGE InstanceSigs #-}
module StateT where

import Control.Monad.Trans.Class
import Control.Monad.IO.Class

newtype StateT s m a =
  StateT { runStateT :: s -> m (a, s) }

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

instance MonadTrans (StateT s) where
  lift ma = StateT $ \s -> do
    a <- ma
    return (a, s)

instance (MonadIO m) => MonadIO (StateT s m) where
  liftIO x = StateT $ \s -> do
    a <- liftIO x
    return (a, s)
