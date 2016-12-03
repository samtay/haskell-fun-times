# Testing
- **unit tests** test the smallest atomic units of software independently
- **spec tests** test specific functions independently as well, but is
often written in terms of human-readable assertions
- **property tests** test the formal properties of programs without requiring formal proofs

In Haskell, the Hspec is the usual choice for spec testing and
QuickCheck library is the usual choice for property testing.
It relies on the type system to randomly generate values that get passed to functions.
Property testing really shines by ensuring you've met the laws of monads and other algebraic structures.

### hspec
A small example:
```haskell
main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "1 + 1 is greater than 1" $ do
      let one = 1 :: Integer
       in (one + one) > one `shouldBe` True

shouldBe :: (Eq a, Show a) => a -> a -> Expectation
(==) :: Eq a => a -> a -> Bool
```

Notice how `shouldBe` is like a super saiyan `==`.

### QuickCheck
QuickCheck gets us closer to *proofs*, by bombing property assertions with data values for a given type. The default number of values is 100. See example:
```haskell
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "x + 1 is always greater than x" $ do
      property $ \x -> x + 1 > (x :: Int)
```
QuickCheck will feed that function 100 random `Int` values to see if the property is ever false.

#### Arbitrary instances
QuickCheck relies on a typeclass called `Arbitrary` and a newtype called `Gen` for generating its random data.
```haskell
arbitrary :: Arbitrary a => Gen a
```
This is just to set a default generator for a type; note that you still have to specify the type to dispatch the right typeclass instance. We get lists of these arbitrary values using `sample` and `sample'`, defined in `Test.QuickCheck`:
```haskell
sample :: Show a => Gen a -> IO ()
sample' :: Gen a -> IO [a]

Prelude> sample (arbitrary :: Gen Int)
0
-2
-1
4
-3
4
2
4
-3
2
-4
```
Note the `IO` monad, which is necessary because it's using a global resource of random values to generate data (i.e., impure!). Here are some other examples:
```haskell
-- concrete types
genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]
genChar :: Gen Char
genChar = elements ['a'..'z']
-- higher kinded types
genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)
-- equal probability
genMaybe :: Arbitrary a => Gen (Maybe a)
genMaybe = do
  a <- arbitrary
  elements [Nothing, Just a]
-- What QuickCheck actually does so you get more Just values
genMaybe' :: Arbitrary a => Gen (Maybe a)
genMaybe' = do
  a <- arbitrary
  frequency [ (1, return Nothing)
            , (3, return (Just a))]

```
Here's what QuickCheck looks like without Hspec:
```haskell
prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x

runQc :: IO ()
runQc = quickCheck prop_additionGreater
```
