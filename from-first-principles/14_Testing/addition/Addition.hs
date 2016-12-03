module Addition where

import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "1 + 1 is greater than 1" $ do
      let one = 1 :: Integer
       in (one + one) > one `shouldBe` True
