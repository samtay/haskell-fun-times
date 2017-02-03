module Data.Log where

import Text.Trifecta
import Control.Applicative ((<|>))
import Data.Time

{-
-- 5 --
Write a parser for a log file format and sum the time spent in
each activity. Additionally, provide an alternative aggregation
of the data that provides average time spent per activity per day.
The format supports the use of comments which your parser
will have to ignore. The # characters followed by a date mark
the beginning of a particular day.
-}

-- Types

-- | Alias for log entry activity description
type Activity = String
type LogS = [LogEntryS]
type LogE = [LogEntryE]

-- | Log entry consists of start time + activity
data LogEntryS =
  LogEntryS UTCTime Activity
  deriving (Eq, Ord, Show)

-- | Log entry consists of elapsed time + activity
data LogEntryE =
  LogEntryE DiffTime Activity
  deriving (Eq, Ord, Show)

-- | Convert a log with start times to a log with elapsed times
--
-- This is a uni-directional conversion, using diffs between start times.
-- Last log entry will have 0 elapsed time
startToElapsed :: LogS -> LogE
startToElapsed = undefined

-- | Get list of activities & average time spent per day
--
-- If log spans 5 days, and Rocket League was played 5 hours the first day,
-- and 10 hours the last day, it would have an entry with average 3 hours.
avgPerActivity :: LogE -> [(Activity, DiffTime)]
avgPerActivity = undefined

-- Parsers

parseLog :: Parser LogS
parseLog = do
  skipComments
  entries <- some $ do
    string "# "
    d  <- parseDay
    skipComments
    ls <- some $ do
      l <- parseLogEntry
      skipComments
      return l
    skipComments
    return $ map (\(t, a) -> LogEntryS (UTCTime d t) a) ls
  return $ concat entries

parseLogEntry :: Parser (DiffTime, Activity)
parseLogEntry = do
  t <- parseDTime
  char ' '
  a <- some (notChar '\n')
  newline
  return (t, a)

parseDTime :: Parser DiffTime
parseDTime = do
  hr <- digits 2
  char ':'
  min <- digits 2
  return $ secondsToDiffTime $ 3600 * hr + 60 * min

digits :: (Num a, Read a) => Int -> Parser a
digits n = read <$> count n digit

parseDay :: Parser Day
parseDay = do
  yr <- integer
  char '-'
  mnth <- digits 2
  char '-'
  day <- digits 2
  return $ fromGregorian yr mnth day

skipComments :: Parser ()
skipComments = do
  spaces
  skipMany parseComment
  spaces

parseComment :: Parser ()
parseComment = do
  string "--"
  skipMany (notChar '\n')
  newline
  return ()
