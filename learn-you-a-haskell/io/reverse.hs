main = do
  line <- getLine
  if null line
     then return () -- "return" is simply the inverse of "<-"
     else do
       putStrLn $ reverseWords line
       main

reverseWords :: String -> String
reverseWords = unwords . map reverse . words
