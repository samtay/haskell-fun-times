module Hypothetical where

import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Data.Functor.Identity

f :: ReaderT String Maybe Int
f = ReaderT $ \r -> Just (length r)

g :: MaybeT (Reader String) Int
g = MaybeT . ReaderT $ \r -> Identity (Just (length r))
