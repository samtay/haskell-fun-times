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

data ExecType = Insert | Update

insertUser :: Connection -> UserRow -> IO ()
insertUser conn = execute conn insertUserQuery

updateUser :: Connection -> UserRow -> IO ()
updateUser conn (_, username, shell, homeDirectory, realName, phone) =
  executeNamed conn updateUserQuery
    [ ":username" := username
    , ":homeDirectory" := homeDirectory
    , ":realName" := realName
    , ":phone" := phone
    , ":shell" := shell ]

updateUserQuery :: Query
updateUserQuery = [r|
UPDATE users
SET
  shell = :shell,
  homeDirectory = :homeDirectory,
  realName = :realName,
  phone = :phone
WHERE username = :username
|]

pastTenseAction :: ExecType -> String
pastTenseAction Insert = "added"
pastTenseAction Update = "updated"

execAction :: ExecType -> Connection -> UserRow -> IO ()
execAction Insert = insertUser
execAction Update = updateUser

main :: IO ()
main = do
  args <- (fmap . fmap) T.pack getArgs
  case args of
    [username, shell, homeDir, realName, phone] ->
      insert Insert (Null, username, shell, homeDir, realName, phone)
    ["--update", username, shell, homeDir, realName, phone] ->
      insert Update (Null, username, shell, homeDir, realName, phone)
    ["-u", username, shell, homeDir, realName, phone] ->
      insert Update (Null, username, shell, homeDir, realName, phone)
    _ -> argsError
    where
      insert method userRow@(_, username, _, _, _, _) = do
        conn <- open "finger.db"
        execAction method conn userRow
        close conn
        putStrLn
          $ unwords ["User", T.unpack username, pastTenseAction method, "successfully."]

argsError :: IO ()
argsError = hPutStrLn stderr helpinfo >> exitFailure
  where helpinfo = [r|
Usage: adduser [OPTION] USERNAME SHELL HOME_DIR REALNAME PHONE
Adds a user to the finger.db database
Example: adduser dexmaster3 /bin/bash /home/dexcaff "Dex Caff" 911

Options:
  -u, --update      Updates existing username
|]

