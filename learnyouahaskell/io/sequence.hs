main = do
  lines <- sequence . take 3 $ repeat getLine
  print lines
