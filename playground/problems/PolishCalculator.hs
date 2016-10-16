calc :: String -> Double
calc input = head . foldl parse [] $ words input
  where parse acc x
          | x == "+" = let (a:b:xs) = acc in (b + a):xs
          | x == "-" = let (a:b:xs) = acc in (b - a):xs
          | x == "*" = let (a:b:xs) = acc in (b * a):xs
          | x == "/" = let (a:b:xs) = acc in (b / a):xs
          | otherwise = (read x :: Double):acc
