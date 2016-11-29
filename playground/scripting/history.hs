#!/usr/bin/env stack
-- stack runghc --resolver lts-7 --install-ghc
import           Data.Char
import           Data.List
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import           System.Environment

main = do
  [fp] <- getArgs
  text <- T.readFile fp
  let histo =
        foldl'
          (\hash line -> M.insertWith (+) (T.length line) 1 hash)
          M.empty
          (T.lines text)
  mapM_ print $ M.toList histo
