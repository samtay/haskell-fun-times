# Rule the types, rule the universe

### 11.2 Data declarations review
Some vocabulary review:

- **nullary** constructors are data or type constructors that take no arguments
- **sum types** denoted by pipes are types that have more than one constructor
- **product types** have data constructors with more than one parameter
- **constants** are just nullary constructors

### 11.6 What’s a type and what’s data?
Some functionality review:

- types are *static* and resolve at *compile time*
- information about types **does not persist through to runtime**

### 11.7 Data constructor arities
**arity** refers to the number of arguments a function or constructor takes

### 11.8 What makes these datatypes algebraic?
Algebraic datatypes in Haskell are algebraic because we can describe the patterns of argument structures using two basic operations: sum and product.

Just like in set theory, the **cardinality** of a datatype is the number of possible values it defines. Given the cardinality of a type, we can deduce the number of possible implementations of a function with a given type signature.

#### Exercises: Cardinality
1. `data PugType = PugData`

  has cardinality 1.
2. `data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited`

  has cardinality 3.
3. `Int16`

  has cardinality `2^16 = 65536`.

### Exercises: For Example
Given `data Example = MakeExample deriving Show`:
1. What is the type of data constructor `MakeExample`? What happens when you request the type of `Example`?

  `MakeExample :: Example`, but `Example` *is* a type and thus does not *have* a type. So GHCi yells at you and says that data constructor `Example` is not in scope.

3. Try making a new datatype like `Example` but with a single type argument added to `MakeExample`, such as `Int`. What has changed when you query `MakeExample` with `:type` in GHCi?

  Then the type of the data constructor changes to a function `MakeExample :: Int -> Example`

Of course, unary constructors will always have the same cardinality as the type they contain.

### 11.9 `newtype`
A `newtype` cannot be a product type, sum type, or contain nullary constructors. It can only ever have a single unary data constructor. However, it has a few advantages over `data` declaration. One advantage is that **newtypes have no runtime overhead** because it reuses the representation of the type it contains.

As an absurd example, if we want to check when we have too many goats or too many cows:
```haskell
tooManyGoats :: Int -> Bool
tooManyGoats n = n > 42

tooManyCows :: Int -> Bool
tooManyCows n = n > 5
```
only representing counts with integers can be unsafe. What if an integer value is representing cows but accidentally gets passed to `tooManyGoats`? We can leverage the type system to be safer, via newtypes:
```haskell
newtype Goats =
  Goats Int deriving (Eq, Show)
newtype Cows =
  Cows Int deriving (Eq, Show)

tooManyGoats :: Goats -> Bool
tooManyGoats (Goats n) = n > 42

-- testing it out
*Main> tooManyGoats (Goats 43)
True
*Main> tooManyGoats (Cows 43)
Error: Couldn't match expected type ‘Goats’ with actual type ‘Cows’
```

#### comparison to type synonym
A `newtype` is similar to a type synonym in that they are identical to the type they are extending/representing and distinction between said type is gone at compile time. It can be useful to humans when reading code, but irrelevant to compiler.

One key difference is that you can **define new typeclass instances** for `newtype`s:
```haskell
class TooMany a where
  tooMany :: a -> Bool
instance TooMany Int where
  tooMany n = n > 42

newtype Goats = Goats Int deriving Show
instance TooMany Goats where
  tooMany (Goats n) = n > 43
```

To reuse the typeclass instance of the underlying type we can use a *language pragma*, aka *extension*, called `GeneralizedNewtypeDeriving`. This allows us to specify user defined typeclass derivations, e.g.
```haskell
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
newtype Goats = Goats Int deriving (Eq, Show, TooMany)
```

### 11.10 Sum types
The cardinality of a *sum type* is the *sum* of the cardinalities of its data constructors.

#### Exercises: Pity the Bool
##### 1. What is the cardinality of the datatype `BigSmall`:
```haskell
data BigSmall = Big Bool | Small Bool
  deriving (Eq, Show)
```
Cardinality is 4.

##### 1. What is the cardinality of the datatype `NumberOrBool`:
```haskell
data NumberOrBool = Numba Int8 | BoolyBool Bool
  deriving (Eq, Show)
```
Cardinality is 258 (256 + 2).

### 11.11 Product types
The cardinality of a *product type* is the *product* of the cardinalities of its inhabitants. This is very straightforward.

#### Record syntax
Records are just product types but provide syntax that desugars into defining convenient accessor methods. They are also very straightforward.
```haskell
data Person = Person { name :: String
                     , age :: Int }
                     deriving (Eq, Show)

-- name :: Person -> String
λ> name (Person "Papu" 5)
"Papu"
```
