module Data.Log where

import Text.Trifecta
import Control.Applicative ((<|>))

{-
-- 5 --
Write a parser for a log file format and sum the time spent in
each activity. Additionally, provide an alternative aggregation
of the data that provides average time spent per activity per day.
The format supports the use of comments which your parser
will have to ignore. The # characters followed by a date mark
the beginning of a particular day.
-}
