{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Main where

import FingerD hiding (User(..), main)

import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (stderr, hPutStrLn)
import qualified Data.Text as T
import Text.RawString.QQ
import Database.SQLite.Simple
import Database.SQLite.Simple.Types

insertUser :: Connection -> UserRow -> IO ()
insertUser conn = execute conn insertUserQuery

main :: IO ()
main = do
  args <- (fmap . fmap) T.pack getArgs
  case args of
    [username, shell, homeDir, realName, phone] -> do
      conn <- open "finger.db"
      insertUser conn (Null, username, shell, homeDir, realName, phone)
      putStrLn $ "User " ++ T.unpack username ++ " added successfully."
    _ -> argsError

argsError :: IO ()
argsError = hPutStrLn stderr helpinfo >> exitFailure
  where helpinfo = [r|
Usage: adduser USERNAME SHELL HOME_DIR REALNAME PHONE
Adds a user to the finger.db database
Example: adduser dexmaster3 /bin/bash /home/dexcaff "Dex Caff" 911
|]

