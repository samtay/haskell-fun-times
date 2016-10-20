class Tofu t where
  tofu :: j a => t a j


{--
  Things to note:
    0. (j a) and (t a j) must each be of  kind *
    1. assume for simplicity that a is of kind *
    2. j must be of                       kind * -> *
    3. t must be of                       kind * -> (* -> *) -> *
  We know this because of the type declaration of function "tofu", as only "concrete" types can be used to describe values, such as within function type declarations.
--}

data Frank a b = Frank {ff :: b a} deriving (Show)
