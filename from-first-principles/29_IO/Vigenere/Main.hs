#!/usr/bin/env stack
{- stack runghc
  --resolver lts-7
  --install-ghc
-}

-- Reusing the Vigenère cipher you wrote back in algebraic datatypes
-- and wrote tests for in testing, make an executable that takes a key
-- and a mode argument. If the mode is -d the executable decrypts the
-- input from stdin and writes the decrypted text to standard out. If
-- the mode is -e the executable blocks on input from standard input
-- and writes the encrypted output to stdout.

-- I could use parseopt-applicative, but since I'm already familiar
-- with it, I'll learn more restricting myself to Prelude.

import Control.Monad (when)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, hWaitForInput, stdin, stderr)
import System.IO.Error (catchIOError, isEOFError)

import Cipher (ceaser, unCeaser)

data Mode = Encrypt | Decrypt
  deriving (Eq, Show)

data Options = Options { mode    :: Mode
                       , timeout :: Int
                       , key     :: Int
                       } deriving (Eq, Show)

main :: IO ()
main = do
  opts <- parseOpts <$> getArgs
  maybe
    showHelp
    (\(Options m t k) -> exec m t k)
    opts

exec :: Mode -> Int -> Int -> IO ()
exec Encrypt t k = handleIO t (ceaser k)
exec Decrypt t k = handleIO t (unCeaser k)

handleIO :: Int -> (String -> String) -> IO ()
handleIO t fn = go ""
  where go input = do
            -- Wait for input
            inputAvailable <- catchIOError
              (hWaitForInput stdin t)
              (\e -> if isEOFError e
                        then return False
                        else ioError e)
            -- Exit if no content after timeout
            when (not inputAvailable && null input)
              $ hPutStrLn stderr "No input given within time limit." >> exitFailure
            -- Collect single char if within timeout
            when inputAvailable
              $ (:input) <$> getChar >>= go
            -- Decode to stdout after timeout
            when (not inputAvailable && not (null input))
              $ putStrLn (fn input) >> exitSuccess

showHelp :: IO ()
showHelp = mapM_ (hPutStrLn stderr)
  [ "Usage: vigenere [OPTION]"
  , "Encrypt or decrypt content from standard input."
  , "Example: echo 'hello world' | vigenere -e"
  , ""
  , "Options:"
  , "  -e          encrypt (default)"
  , "  -d          decrypt"
  , "  -t<NUM>     set timeout (default 5 seconds)"
  ]

defaultOptions :: Options
defaultOptions = Options Encrypt 5000 2

parseOpts :: [String] -> Maybe Options
parseOpts = foldr go (Just defaultOptions)
  where go _ Nothing               = Nothing
        go "-e" (Just os)          = Just os {mode = Encrypt}
        go "-d" (Just os)          = Just os {mode = Decrypt}
        go ('-':'t':num) (Just os) = Just os {timeout = read num * 1000}
        go ('-':'k':num) (Just os) = Just os {key = read num}
        go _ _                     = Nothing
