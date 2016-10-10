import System.IO
import Data.Char

main = do
  contents <- readFile "input.txt"
  writeFile "output.txt" (map toUpper contents)

{--
This is just short for 
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  whandle <- openFile "output.txt" WriteMode
  hPutStrLn whandle (map toUpper contents)
--}
