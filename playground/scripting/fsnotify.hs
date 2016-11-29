#!/usr/bin/env stack
-- stack runghc --resolver lts-7 --install-ghc --package fsnotify
{-# LANGUAGE OverloadedStrings #-} -- for FilePath literals

import System.FSNotify
import Control.Concurrent (threadDelay)
import Control.Monad (forever)

main =
  withManager $ \mgr -> do
    -- start watching job
    watchDir
      mgr          -- manager
      "."          -- directory to watch
      (const True) -- predicate (always)
      print        -- action

    -- sleep forever (until ctrl+c)
    forever $ threadDelay 1000000
