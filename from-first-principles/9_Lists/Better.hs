module Better where

-- 3. Parameterize the splitting char from exercises 1 & 2.
splitBy :: Char -> String -> [String]
splitBy c cs = reverse $ go [] (dropWhile (==c) cs)
  where go ws "" = ws
        go ws s  = let next  = takeWhile (/=c) s
                       rest = (dropWhile (==c)) . (dropWhile (/=c)) $ s
                       in go (next:ws) rest
