{--

1    type: Just for synonyms, giving another name to existing type
           These dont give value constructors or anything, they just allow us to use a new
           name for existing types in our type annotations and whatnot, for clarity.

2 newtype: Wrapping existing types
           Usually to make them instances of certain type classes
           Not automatically made an instance of the same type classes of original type

3    data: Making custom data types
           Ultimate flexibility, can have as many constructors and fields as you wish
--}
{--

The newtype keyword is made for wrapping functionality of existing types.

It differs from the data keyword because it is restricted to a single value
constructor that has a single field (what we are wrapping). If this is the
use case, newtype is preferred for speed.

ZipList a is defined as:

newtype ZipList a = ZipList { getZipList :: [a] }
--}
{--

Using newtype to make type class instances:

What if we want to implement the Functor type class for tuples like (a,b)
where functions act on the first element of the tuple? This is tough with the
`instance Functor (* -> *) where`
syntax.

We can solve this with a newtype, so that the second type parameter
represents the first component in the tuple:
--}
newtype Pair b a = Pair { getPair :: (a,b) }

instance Functor (Pair c) where
  fmap f (Pair (x,y)) = Pair (f x, y)


newtype CoolBool = CoolBool { getCoolBool :: Bool }

helloMe :: CoolBool -> String
helloMe (CoolBool _) = "hello"

{--

With newtype, evaluation is kept lazy and internally can represent the new type as it does
with original values, only keeping track of the distinction. Because Haskell knows types made
with newtype can only have one constructor, it doesn't have to evaluate the value passed to
helloMe to make sure that it confroms to (CoolBool _) pattern, since newtype types can only
have one possible value constructor and one field. So
`helloMe undefined`
returns "hello" no problemo.

If instead defined with data keyword:

data CoolBool = CoolBool { getCoolBool :: Bool }

`helloMe undefined`
throws exception because it is forced to convert the value to the pattern.
--}

