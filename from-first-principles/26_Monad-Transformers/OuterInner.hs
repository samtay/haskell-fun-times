module OuterInner where

import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader

-- We only need to use return once
-- because it's one big Monad
embedded :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded = return 1

unwrap :: () -> IO (Either String (Maybe Int))
unwrap = (runReaderT . runExceptT . runMaybeT) embedded
