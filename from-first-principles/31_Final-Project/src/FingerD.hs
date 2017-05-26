{-# LANGUAGE OverloadedStrings, QuasiQuotes, RecordWildCards #-}
module FingerD where

import Control.Exception
import Control.Monad (void, forever)
import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Typeable

import Database.SQLite.Simple hiding (close, bind)
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.Types
import Network.Socket hiding (close, recv, bind)
import qualified Network.Socket as Network
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Network.Socket.ByteString (recv, sendAll)
import Text.RawString.QQ

port :: String
port = "79"

data User =
  User { userId :: Integer
       , username :: Text
       , shell :: Text
       , homeDirectory :: Text
       , realName :: Text
       , phone :: Text
       } deriving (Eq, Show)

instance FromRow User where
  fromRow = User <$> field
                  <*> field
                  <*> field
                  <*> field
                  <*> field
                  <*> field

instance ToRow User where
  toRow (User id_ username shell homeDir realName phone)
    = toRow (id_, username, shell, homeDir, realName, phone)

createUsersTable :: Query
createUsersTable = [r|
CREATE TABLE IF NOT EXISTS users
  (id INTEGER PRIMARY KEY AUTOINCREMENT,
   username TEXT UNIQUE,
   shell TEXT,
   homeDirectory TEXT,
   realName TEXT,
   phone TEXT)
|]

insertUserQuery :: Query
insertUserQuery = "INSERT INTO users VALUES (?, ?, ?, ?, ?, ?)"

allUsersQuery :: Query
allUsersQuery = "SELECT * FROM users"

getUserQuery :: Query
getUserQuery = "SELECT * FROM users WHERE username = ?"

data DuplicateData = DuplicateData
  deriving (Eq, Show, Typeable)

instance Exception DuplicateData

type UserRow = (Null, Text, Text, Text, Text, Text)

createDatabase :: IO ()
createDatabase = do
  conn <- open "finger.db"
  execute_ conn createUsersTable
  execute conn insertUserQuery meRow
  rows <- query_ conn allUsersQuery
  mapM_ print (rows :: [User])
  SQLite.close conn
    where meRow :: UserRow
          meRow = (Null, "samtay", "/bin/zsh", "/home/samtay", "Sam Tay", "631-291-3866")

getUser :: Connection -> Text -> IO (Maybe User)
getUser conn username = do
  results <- query conn getUserQuery (Only username)
  case results of
    [] -> return Nothing
    [user] -> return $ Just user
    _ -> throwIO DuplicateData

returnUsers :: Connection -> Socket -> IO ()
returnUsers dbConn soc = do
  rows <- query_ dbConn allUsersQuery
  let usernames = map username rows
      newlineSeparated = T.concat $ intersperse "\n" usernames
  sendAll soc (encodeUtf8 newlineSeparated)

formatUser :: User -> ByteString
formatUser User{..}
  = BS.concat [ "Login: ", e username, "\t\t\t\t"
              , "Name: ", e realName, "\n"
              , "Directory: ", e homeDirectory, "\t\t\t"
              , "Shell: ", e shell, "\n" ]
  where e = encodeUtf8

returnUser :: Connection -> Socket -> Text -> IO ()
returnUser dbConn soc username =
  getUser dbConn (T.strip username)
    >>= maybe
        (void $ putStrLn ("Couldn't find matching user for username: " ++  (show username)))
        (sendAll soc . formatUser)

handleQuery :: Connection -> Socket -> IO ()
handleQuery dbConn soc = do
  msg <- recv soc 1024
  case msg of
    "\r\n" -> returnUsers dbConn soc
    name -> returnUser dbConn soc (decodeUtf8 name)

handleQueries :: Connection -> Socket -> IO ()
handleQueries dbConn sock = forever $ do
  (soc, _) <- accept sock
  putStrLn "Got connection, handling query"
  handleQuery dbConn soc
  Network.close soc

main :: IO ()
main = withSocketsDo $ do
  addrinfos <- getAddrInfo
    (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
    Nothing
    (Just port)
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  Network.bind sock (addrAddress serveraddr)
  listen sock 1
  conn <- open "finger.db"
  handleQueries conn sock
  SQLite.close conn
  Network.close sock
