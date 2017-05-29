{-# LANGUAGE OverloadedStrings, QuasiQuotes, RecordWildCards #-}
module FingerD where

import Control.Exception
import Control.Monad (forever)
import Control.Concurrent (forkIO)
import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Typeable

import Data.Aeson hiding (Null)
import Database.SQLite.Simple hiding (close, bind)
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.Types
import Network.Socket hiding (close, recv, bind)
import qualified Network.Socket as Network
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Network.Socket.ByteString (recv, sendAll)
import Text.RawString.QQ

data User =
  User { userId :: Integer
       , username :: Text
       , shell :: Text
       , homeDirectory :: Text
       , realName :: Text
       , phone :: Text
       } deriving (Eq, Show)

data UserInsert =
  UserInsert Text Text Text Text Text
  deriving (Eq, Show)

data DuplicateData = DuplicateData
  deriving (Eq, Show, Typeable)

instance Exception DuplicateData

type UserRow = (Null, Text, Text, Text, Text, Text)

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

instance FromJSON UserInsert where
  parseJSON (Object v) = UserInsert
    <$> v .: "username"
    <*> v .: "shell"
    <*> v .: "homeDirectory"
    <*> v .: "realName"
    <*> v .: "phone"
  parseJSON _ = error "Invalid user JSON"

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

returnUsers :: Connection -> IO (ByteString)
returnUsers dbConn =
  encodeUtf8 . T.concat . intersperse "\n" . map username
    <$> query_ dbConn allUsersQuery

returnUser :: Connection -> Text -> IO (Maybe ByteString)
returnUser dbConn username =
  getUser dbConn (T.strip username)
    >>= maybe noUserError (return . Just . formatUser)
  where
    noUserError =
      putStrLn ("Couldn't find matching user for username: " ++ show username)
        >> return Nothing


insertUserFromJSON :: Connection -> ByteString -> IO ()
insertUserFromJSON dbConn userJSON =
  case decodeStrict userJSON of
    Nothing -> putStrLn "Failed to decode user JSON."
    Just (UserInsert u s h n p) -> do
      result <- try $ execute dbConn insertUserQuery (Null, u, s, h, n, p)
      either
        sqlErrorHandler
        (const $ putStrLn $ "Added user " ++ T.unpack u ++ " successfully.")
        result
  where
    sqlErrorHandler :: SQLError -> IO ()
    sqlErrorHandler e = putStrLn $ "Error inserting user: " ++ T.unpack (sqlErrorDetails e)

formatUser :: User -> ByteString
formatUser User{..}
  = BS.concat [ "Login: ", e username, "\t\t\t\t"
              , "Name: ", e realName, "\n"
              , "Directory: ", e homeDirectory, "\t\t\t"
              , "Shell: ", e shell, "\n" ]
  where e = encodeUtf8

handleQuery :: Connection -> Socket -> IO ()
handleQuery dbConn soc = do
  msg <- recv soc 1024
  mresponse <- case msg of
    "\r\n" -> Just <$> returnUsers dbConn
    name -> returnUser dbConn (decodeUtf8 name)
  maybe (return ()) (sendAll soc) mresponse

handleQueries :: Connection -> Socket -> IO ()
handleQueries dbConn sock = forever $ do
  (soc, _) <- accept sock
  putStrLn "Got connection, handling query"
  handleQuery dbConn soc
  Network.close soc

handleInserts :: Connection -> Socket -> IO ()
handleInserts dbConn sock = forever $ do
  (soc, _) <- accept sock
  putStrLn "Got connection, handling insert"
  msg <- recv soc 1024
  insertUserFromJSON dbConn msg
  Network.close soc

main :: IO ()
main = withSocketsDo $ do
  putStrLn "Starting up query server on port 79, insert server on port 1026..."
  _ <- forkIO $ setupServer "79" handleQueries
  setupServer "1026" handleInserts

setupServer :: ServiceName -> (Connection -> Socket -> IO a) -> IO ()
setupServer port handler = do
  addrinfos <- getAddrInfo
    (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
    Nothing
    (Just port)
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  Network.bind sock (addrAddress serveraddr)
  listen sock 1
  conn <- open "finger.db"
  _ <- handler conn sock
  SQLite.close conn
  Network.close sock
