main = do
  putStrLn "What's your first name?"
  firstName <- getLine
  putStrLn "What's your last name?"
  lastName <- getLine
  putStrLn $ "Hi " ++ firstName ++ " Mae " ++ lastName ++ "!"
