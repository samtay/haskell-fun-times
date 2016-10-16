import Data.Monoid
{--

YAY more math. Monoid is a generalization of groups (groups without guaranteed inverses)

class Monoid m where
  mempty :: m
  mappend :: m -> m -> m
  mconcat :: [m] -> m
  mconcat = foldr mappend mempty

Laws:
  1. mempty `mappend` x = x
  2. x `mappend` mempty = x
  3. (x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)

--}

{-- lists are monoids
instance Monoid [a] where
  mempty = []
  mappend = (++)
--}

{--

Since Num type can be a monoid under both * and +, Data.Monoid exports two types:

newtype Product a = Product { getProduct :: a }
  deriving (Eq, Ord, Read, Show, Bounded)

instance Num a => Monoid (Product a) where
  mempty = Product 1
  Product x `mappend` Product y = Product (x * y)

-- similar implementation for addition

Parallel to this, Any and All are Monoids based on Bool under (||, False) and (&&, True)
respectively

--}

lengthCompare :: String -> String -> Ordering
lengthCompare x y = (length x `compare` length y) `mappend`
                    (vowels x `compare` vowels y) `mappend`
                    (x `compare` y)
  where vowels = length . filter (`elem` "aeiou")

firstOfList :: [Maybe a] -> Maybe a
firstOfList = getFirst . mconcat . map First

lastOfList :: [Maybe a] -> Maybe a
lastOfList = getLast . mconcat . map Last
