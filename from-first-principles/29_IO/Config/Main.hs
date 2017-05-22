#!/usr/bin/env stack
{- stack runghc
  --resolver lts-7
  --install-ghc
  --package trifecta
  --package raw-strings-qq
  --package hspec
  --package containers
-}
module Main where

--Reusing the INI parser from the Parsing chapter, parse a directory of
--ini config files into a Map whose key is the filename and whose value
--is the result of parsing the INI file. Only parse files in the directory
--that have the file extension .ini.

--Again, after writing Dockmaster, I could do this quickly via shelly
--but will learn more by restricting myself to base.

import Data.Map (Map)

import Control.Monad ((>=>))
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (FilePath, stderr, hPutStrLn)

import Data.Ini hiding (main)

main :: IO ()
main =
  (parseArgs <$> getArgs)
    >>= maybe argError (parseFilesInDir >=> showResult)

parseFilesInDir :: FilePath -> IO (Map FilePath (Maybe Config))
parseFilesInDir = undefined

showResult :: Map FilePath (Maybe Config) -> IO ()
showResult = undefined

argError :: IO ()
argError =
  hPutStrLn stderr "This program accepts exactly a single directory argument."
    >> exitFailure

parseArgs :: [String] -> Maybe FilePath
parseArgs [d] = Just d
parseArgs _   = Nothing
