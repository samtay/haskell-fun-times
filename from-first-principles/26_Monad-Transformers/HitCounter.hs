#!/usr/bin/env stack
{- stack runghc
  --resolver lts-7
  --install-ghc
  --package scotty
-}
{-# LANGUAGE OverloadedStrings #-}
{-
Code is missing and broken. Your task is to make it work, whatever
is necessary.

Once it’s running, you should be able to
bump the counts like so:

$ curl localhost:3000/woot
<h1>Success! Count was: 1</h1>
$ curl localhost:3000/woot
<h1>Success! Count was: 2</h1>
$ curl localhost:3000/blah
<h1>Success! Count was: 1</h1>
-}
module HitCounter where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.IORef
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import System.Environment (getArgs)
import Web.Scotty.Trans

data Config =
  Config { counts :: IORef (M.Map Text Integer)
         , prefix :: Text
         }

-- Stuff inside ScottyT is, except for things that escape
-- via IO, effectively read-only so we can't use StateT.
-- It would overcomplicate things to attempt to do so and
-- you should be using a proper database for production
-- applications.
type Scotty = ScottyT Text (ReaderT Config IO)
type Handler = ActionT Text (ReaderT Config IO)

bumpBoomp :: Text
         -> M.Map Text Integer
         -> (M.Map Text Integer, Integer)
bumpBoomp k m =
  let newM = M.insertWith (+) k 1 m
      newC = newM M.! k
   in (newM, newC)

app :: Scotty ()
app =
  get "/:key" $ do
    unprefixed       <- param "key"
    (Config ref pre) <- lift $ ask
    let k = mappend pre unprefixed
    newC <- liftIO $ do
      oldM <- readIORef ref
      let (newM, newC) = bumpBoomp k oldM
      writeIORef ref newM
      return newC
    html $ mconcat [ "<h1>Success! Count was: "
                   , TL.pack $ show newC
                   , "</h1>"
                   ]

main :: IO ()
main = do
  [prefixArg] <- getArgs
  counter <- newIORef M.empty
  let config = Config counter (TL.pack prefixArg)
      runR rRes = runReaderT rRes config
  scottyT 3000 runR app
