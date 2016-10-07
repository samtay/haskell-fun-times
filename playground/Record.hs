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

data Vector a = Vector a a a deriving (Show)
 -- Note (Vector a) is the type constructor
    --   while (Vector a a a) is the value constructor

-- Note this type declaration takes the `type constuctor` with its single parameter
scalarMult :: (Num t) => Vector t -> Vector t -> t
(Vector i j k) `scalarMult` (Vector l m n) = i*l + j*m + k*n
-- while the pattern matching is done against the `value constructor` with its three parameters

