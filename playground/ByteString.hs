import System.Environment
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as S

main = do
  (fileName1:fileName2:_) <- getArgs
  copyFile fileName1 fileName2

-- custom "copyFile" func to demonstrate ByeString package
copyFile :: FilePath -> FilePath -> IO ()
copyFile source dest = do
  contents <- B.readFile source
  B.writeFile dest contents
