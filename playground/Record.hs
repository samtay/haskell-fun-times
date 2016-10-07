module Records
( Person(..)
, Car(..)
, Vector(..)
) where

data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     , height :: Float
                     , flavor :: String
                     } deriving (Show)

data Car = Car {company :: String, model :: String, year :: Int} deriving (Show)

data Vector = Vector Int Int Int deriving (Show)
