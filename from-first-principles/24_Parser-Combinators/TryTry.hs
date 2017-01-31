{-# LANGUAGE OverloadedStrings #-}
module TryTry where

import Text.Trifecta
import Control.Applicative ((<|>))
import Data.Ratio ((%))
import Control.Monad (forM_)
import Data.Maybe (fromMaybe, isNothing)

-- Taken from TextFraction.hs
parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  eof
  case denominator of
    0 -> fail "Denominator cannot be zero"
    _ -> return (numerator % denominator)

-- Allows for .X or X. by assuming 0.X and X.0 respectively.
parseDecimal :: Parser Rational
parseDecimal = do
  wholeNum   <- fromIntegral <$$> optional decimal
  decimalNum <- fromIntegral <$$> optional (char '.' >> decimal)
  eof
  if and $ isNothing <$> [wholeNum, decimalNum]
     then unexpected "decimal not found"
     else return $ (fromMaybe 0 wholeNum) + (maybe 0 mkDec decimalNum)
       where
         mkDec x = if x < 1 then x else mkDec (x / 10)
         (<$$>)  = fmap . fmap

-- Note that parseFraction <|> parseDecimal alone will cause errors because
-- parseFraction will start to consume "1.234" and then error on unexpected '.'
parseRational :: Parser Rational
parseRational = try parseFraction <|> parseDecimal

fixture :: [(String, String)]
fixture =
  [ ("fail","abc")
  , ("fail","1/abc")
  , ("fail","1.23abc")
  , ("pass","123")
  , ("pass","1/23")
  , ("pass","1.23")
  , ("pass","0.23")
  , ("pass","1.0")
  , ("pass","00")
  , ("pass","00.0") ]

main :: IO ()
main = forM_ fixture $ \(res, val) -> do
  putStrLn $ res ++ ": " ++ val
  print $ parseString parseRational mempty val
