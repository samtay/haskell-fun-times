module Main where

import Test.QuickCheck hiding (Result(..))
import Test.Hspec
import Text.Trifecta
import Data.Log
import Data.Maybe (isJust)
import Data.Time

-- TODO quickcheck generators

main :: IO ()
main = do
  hspec specs
  -- some quickcheck stuff here

specs :: Spec
specs =
  describe "Parsing log file" $ do
    r <- runIO $ parseFromFile parseLog "test/fixtures/chrisdiary.txt"

    it "can parse Chris's diary example" $
      r `shouldSatisfy` isJust

    it "parsed first entry accurately" $
      head <$> r
        `shouldBe` Just (LogEntryS (UTCTime (fromGregorian 2025 2 5) (8*3600)) "Breakfast")

    it "parsed last entry accurately" $
      last <$> r
        `shouldBe` Just (LogEntryS (UTCTime (fromGregorian 2025 2 7) (22*3600)) "Sleep")

    it "ignores comments in activities" $
      (!!11) <$> r
        `shouldBe` Just (LogEntryS (UTCTime (fromGregorian 2025 2 7) (8*3600)) "Breakfast")
