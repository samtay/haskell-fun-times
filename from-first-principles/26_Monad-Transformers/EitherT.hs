{-# LANGUAGE InstanceSigs #-}
module EitherT where

import Control.Monad.Trans.Class
import Control.Monad (liftM)

newtype EitherT e m a =
  EitherT { runEitherT :: m (Either e a) }

instance (Functor f) =>
  Functor (EitherT e f) where
    fmap f (EitherT ma) = EitherT $ (fmap . fmap) f ma

instance (Applicative f) =>
  Applicative (EitherT e f) where
    pure = EitherT . pure . pure

    (EitherT mf) <*> (EitherT ma) =
      EitherT $ (<*>) <$> mf <*> ma

instance (Monad m) =>
  Monad (EitherT e m) where
    return = pure

    (EitherT ma) >>= f = EitherT $ do
      -- a :: Either e a
      a <- ma
      case a of
        Left e  -> return $ Left e
        Right x -> runEitherT $ f x

instance MonadTrans (EitherT e) where
  lift = EitherT . liftM Right

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT ma) = EitherT $ swapEither <$> ma

swapEither :: Either e a -> Either a e
swapEither (Left x)  = Right x
swapEither (Right x) = Left x

eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT f g (EitherT ma) = ma >>= either f g
