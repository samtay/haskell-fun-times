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

### quickcheck

