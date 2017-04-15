-- 4 Kyu
module RangeExtractor.JorgeVS.Kata where

import Data.List (intercalate)

solution :: [Int] -> String
solution = intercalate "," . go [] []
  where go acc [] []     = acc                  -- empty case
        go acc rs []     = acc ++ [render rs]   -- terminating case
        go acc [] (x:xs) = go acc [x] xs        -- init case
        go acc rs (x:xs) = if last rs + 1 == x  -- inductive case
                              then go acc (rs ++ [x]) xs
                              else go (acc ++ [render rs]) [x] xs

        render [] = ""
        render [r] = show r
        render rs = let sep = if length rs == 2 then "," else "-"
                     in intercalate sep . map show $ [head rs, last rs]
