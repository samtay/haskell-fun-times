{-# LANGUAGE QuasiQuotes #-}
module Main where

import Data.Log
import Text.Trifecta
import Data.Time
import Text.RawString.QQ (r)
import Test.QuickCheck hiding (Result(..))
import Test.Hspec

import Data.Maybe (isJust)

-- TODO quickcheck generators

main :: IO ()
main = do
  hspec specs
  -- some quickcheck stuff here

specs :: Spec
specs = do
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

  describe "Aggregating average times" $ do
    it "totals rocket league play time: 23 hours" $
      (maybeSuccess (parseString parseLog mempty exampleRL)
        >>= return . totalPerActivity . startToElapsed
        >>= lookup "Rocket League"
        >>= return . (/3600))
        `shouldBe` (Just 23)
    it "averages time with Brice: 12 mins / day" $
      (maybeSuccess (parseString parseLog mempty exampleRL)
        >>= return . avgPerActivity
        >>= lookup "Show Brice parser stuff"
        >>= return . (/60))
        `shouldBe` (Just 12)

exampleRL :: String
exampleRL = [r|
# 2017-02-06
09:00 Show Brice parser stuff
10:00 Chill
15:00 Get fired
16:00 Rocket League
19:00 Haskell
21:00 Rocket League -- again

# 2017-02-07
02:30 Sleep
11:00 Rocket League
15:30 Haskell
22:30 Rocket League

# 2017-02-08
01:30 Sleep

# 2017-02-10
13:00 Rocket League
20:00 Sleep
|]

-- | Swap types for easy Eq instance
maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing
