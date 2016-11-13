# Lists

Lists serve two purposes:
- as collection/purality of values
- as infinite series acting as stream datatype

### 9.2 The list datatype
```haskell
data [] a = [] | a : [a]
```
- So `[]` is the type constructor for lists and the data constructor for the empty list.
- The `[]` data constructor is a *nullary* constructor as it takes no arguments.
- The `:` data constructor is an infix operator called *cons*, taking a value of type `a` and a list of type `[a]` (recursive definition).
- The list datatype itself is a sum type, but the data constructor `:` is a product type since it takes two arguments.

### 9.4 List's syntactic sugar
Lists are often discussed in terms of *cons cells* and *spines*. The cons cell is the conceptual space that values may inhabit, while the spine is the connectice structure holding the cons cells together and in place. This structure actually nests cons cells rather them ordering them horizontally. The syntactic sugar for `(1 : 2 : 3 : []) == [1,2,3,4]` obscures this construction, but diferent functions treat the spine / cons cells differently, so it's important to keep in mind.

### 9.5 Using ranges to construct lists
```haskell
λ> [1..10]
[1,2,3,4,5,6,7,8,9,10]

λ> [3..10]
[3,4,5,6,7,8,9,10]

-- Using three numbers [x,y..z] will go from x to z, in gaps of (y - z)
λ> [2,4..10]
[2,4,6,8,10]

λ> [1,3..10]
[1,3,5,7,9]
```
Underlying functions of range syntax are:
```haskell
-- infinite starting at x
-- [x..]
enumFrom :: Enum a => a -> [a]

-- infinite  starting at x in gaps of y - x
-- [x,y..]
enumFromThen :: Enum a => a -> a -> [a]

-- finite from x to z
-- [x..z]
enumFromTo :: Enum a => a -> a -> [a]

-- finite from x to z in gaps of (y - x)
-- [x,y..z]
enumFromThenTo :: Enum a => a -> a -> a -> [a]
```

