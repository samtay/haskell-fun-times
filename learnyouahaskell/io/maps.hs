import Control.Monad

{--
mapM f list === sequence . map f list
mapM_ throws away result

forM a b === mapM b a
--}

main = do
  colors <- forM [1,2,3,4] (\a -> do
    putStrLn $ "Which color is mapped to " ++ show a ++ "?"
    color <- getLine
    return color)
  putStrLn "The colors that you associate with 1, 2, 3, and 4 are: "
  mapM_ putStrLn colors
