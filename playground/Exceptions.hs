import System.Environment
import System.IO

{--
Testing file existence directly via doesFileExist
import System.Directory

main = do (fileName:_) <- getArgs
          fileExists <- doesFileExist fileName
          if fileExists
             then do contents <- readFile fileName
                     putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"
             else do  putStrLn "The file doesn't exist!"
--}

-- catching exceptions
import System.IO.Error
main = try `catchIOError` handler

try :: IO ()
try = do (fileName:_) <- getArgs
         contents <- readFile fileName
         putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"

handler :: IOError -> IO ()
handler _ = putStrLn "<[ insert unuseful error message ]>"
