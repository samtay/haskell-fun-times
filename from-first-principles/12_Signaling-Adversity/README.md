# Signaling adversity
Our first look at monads! I already know `Maybe` and `Either` very well from LYAH, so the notes will be brief.

### 12.3 Bleating either
A primitive attempt at catching errors for inputs when creating "People":
```haskell
type Name = String
type Age = Integer
data Person = Person Name Age deriving Show
data PersonInvalid = NameEmpty | AgeTooLow
                     deriving (Eq, Show)
mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age >= 0 = Right $ Person name age -- success
  | name == "" = Left NameEmpty -- name error
  | otherwise = Left AgeTooLow -- age error
```

Next let's adapt to catch multiple errors. Notice that instead of checking for valid data all at once, we make separate checking functions and then combine results:
```haskell
-- Keep types from above, but add the following type alias:
type ValidatePerson a = Either [PersonInvalid] a

-- Checking functions
ageOkay :: Age -> Either [PersonInvalid] Age
ageOkay age = case age >= 0 of
  True -> Right age
  False -> Left [AgeTooLow]

nameOkay :: Name -> Either [PersonInvalid] Name
nameOkay name = case name /= "" of
  True -> Right name
  False -> Left [NameEmpty]
```

Now, since we haven't learned how to combine monadic functions, let's naiively use all of these checks via some ugly pattern matching:
```haskell
mkPerson :: Name -> Age -> ValidatePerson Person
mkPerson name age =
  mkPerson' (nameOkay name) (ageOkay age)

mkPerson' :: ValidatePerson Name -> ValidatePerson Age -> ValidatePerson Person
mkPerson' (Right nameOk) (Right ageOk) = Right (Person nameOk ageOk)
mkPerson' (Left badName) (Left badAge) = Left (badName ++ badAge)
mkPerson' (Left badName) _             = Left badName
mkPerson' _ (Left badAge)              = Left badAg
```

Later on, we'll get more elegant:
```haskell
mkPerson :: Name -> Age -> Validation [PersonInvalid] Person
mkPerson name age = liftA2 Person (nameOkay name) (ageOkay age)
```

### 12.4 Kinds, a thousand stars in your types
One noteworthy feature of Haskell is that it has *higher-kinded* types (term is analogous to higher-order functions), which take more types as arguments. `Maybe` and `Either` are obvious examples, while `Int` is not higher-kinded, but just a simple type constant.

#### Lifted and unlifted types
Kind `*` is the kind of all standard lifted types, while types that have the kind `#` are unlifted. A **lifted datatype** is any that can be inhabited by *bottom* (this includes any datatypes that we can define as Haskell developers). **Unlifted types** are any type which *cannot* be inhabited by bottom. Examples include native machine types and raw pointers.

Interestingly, `newtype` is a special case having kind `*` while being unlifted. This is because their representation is identical to the type they contain, hence not creating any new pointer. Therefore the newtype itself is not inhabited by bottom, only the type it contains is.

### 12.5 Chapter Exercises
1. What is the kind of `a` in `id :: a -> a`?

 `:kind a === *
2. What are the kinds of `a` and `f` in `r :: a -> f a`?

 `:kind a === *`
 `:kind f === * -> *`

