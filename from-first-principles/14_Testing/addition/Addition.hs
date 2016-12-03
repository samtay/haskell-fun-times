module Addition where

import Test.Hspec
import Test.QuickCheck

-- Hspec
main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "1 + 1 is greater than 1" $ do
      let one = 1 :: Integer
       in (one + one) > one `shouldBe` True
    it "x+1 is always greater than x" $ do
      property $ \x -> x + 1 > (x :: Int) -- with QuickCheck

-- QuickCheck
prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x

-- >>> runQc
-- +++ OK, passed 100 tests
runQc :: IO ()
runQc = quickCheck prop_additionGreater
