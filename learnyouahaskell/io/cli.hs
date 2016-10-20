import System.Environment
import Data.List

main = do
  args <- getArgs
  progName <- getProgName
  putStrLn "The args are:"
  mapM putStrLn args
  putStrLn "The name is:"
  putStrLn progName
