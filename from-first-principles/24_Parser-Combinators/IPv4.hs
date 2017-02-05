#!/usr/bin/env stack
{- stack runghc
  --resolver lts-7
  --install-ghc
  --package hspec
  --package trifecta
-}
module IPv4 where

import Text.Trifecta
import Data.Word
import Control.Applicative ((<|>))
import Control.Monad (when)
import Test.Hspec

{-
-- 6 --
Write a parser for IPv4 addresses.

A 32-bit word is a 32-bit unsigned int. Lowest value is 0 rather
than being capable of representing negative numbers, but the
highest possible value in the same number of bits is twice as
high.

Word32 is an appropriate and compact way to represent IPv4
addresses. You are expected to figure out not only how to parse
the typical IP address format, but how IP addresses work numer-
ically insofar as is required to write a working parser. This will
require using a search engine unless you have an appropriate
book on internet networking handy.
Example IPv4 addresses and their decimal representations:

172.16.254.1 -> 2886794753
204.120.0.15 -> 3430416399
-}

-- Types

data IPAddress =
  IPAddress Word32
  deriving (Eq, Ord, Show)

-- Parsing funcs

-- | `sepBy` doesn't let you limit count -_-
parseIPAddress :: Parser IPAddress
parseIPAddress = do
  o1 <- octet
  os <- count 3 $ char '.' >> octet
  return . IPAddress . sum $
    zipWith (\o n -> o * 256^n) (o1:os) (reverse [0..3])

octet :: Parser Word32
octet = do
  x <- integer
  when (x < 0 || x > 255) $ unexpected "integer outside range [0,255]"
  return (fromInteger x)

-- Testing

-- This is done because Result doesn't have Show instance
toMaybe :: Result a -> Maybe a
toMaybe (Success x) = Just x
toMaybe _           = Nothing

testIP :: String -> IPAddress -> Spec
testIP s ip = it ("parses " ++ s) $ do
  let res = parseString parseIPAddress mempty s
  case res of
    (Success _)    -> return ()
    (Failure info) -> print info
  toMaybe res `shouldBe` Just ip

-- Runtime

main :: IO ()
main = hspec $
  describe "Parsing IP Addresses" $
    mapM_ (uncurry testIP) $
      [ ("192.168.1.16", IPAddress 3232235792)
      , ("23.125.48.166", IPAddress 394080422)
      , ("23.125.48.166", IPAddress 394080422)
      , ("172.16.0.16", IPAddress 2886729744)
      ]
