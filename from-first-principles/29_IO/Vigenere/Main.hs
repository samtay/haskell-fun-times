#!/usr/bin/env stack
{- stack runghc
  --resolver lts-8
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

import System.Environment (getArgs)
import System.IO (hPutStrLn, hGetChar, hWaitForInput, stdout, stdin, stderr)

import Cipher (ceaser, unCeaser)

data Mode = Encrypt | Decrypt
  deriving (Eq, Show)

data Options = Options { mode    :: Maybe Mode
                       , timeout :: Int
                       , key     :: Int
                       } deriving (Eq, Show)

main :: IO ()
main = do
  (Options mm t k) <- parseOpts <$> getArgs
  maybe
    showHelp
    (\m -> exec m t k)
    mm

exec :: Mode -> Int -> Int -> IO ()
exec Encrypt t k = interact (ceaser k)
exec Decrypt t k = interact (unCeaser k)

showHelp :: IO ()
showHelp = mapM_ (hPutStrLn stderr)
  [ "Usage: vigenere [OPTION]"
  , "Encrypt or decrypt content from standard input."
  , "Example: echo 'hello world' | vigenere -e"
  , ""
  , "Options:"
  , "  -e          encrypt"
  , "  -d          decrypt"
  , "  -t<NUM>     set timeout (default 5 seconds)"
  ]

defaultOptions :: Options
defaultOptions = Options Nothing 5 2

parseOpts :: [String] -> Options
parseOpts = foldr go defaultOptions
  where go "-e" os = os {mode = Just Encrypt}
        go "-d" os = os {mode = Just Decrypt}
        go ('-':'t':num) os = os {timeout = read num}
        go ('-':'k':num) os = os {key = read num}
        go _ os = os
