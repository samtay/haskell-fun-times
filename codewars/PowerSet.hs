-- 5 Kyu
module Power where

power :: [a] -> [[a]]
power = ([]:) . go
  where go []     = []
        go (x:xs) = let f ys rs   = ys : (x : ys) : rs
                     in [x] : foldr f [] (go xs)
