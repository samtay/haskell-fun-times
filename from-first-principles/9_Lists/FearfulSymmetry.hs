module FearfulSymmetry where

-- 1. Implement `words` using `takeWhile` and `dropWhile`
words' :: String -> [String]
words' sentence = reverse $ go [] (dropWhile (==' ') sentence)
  where go ws "" = ws
        go ws s  = let next  = takeWhile (/=' ') s
                       rest = (dropWhile (==' ')) . (dropWhile (/=' ')) $ s
                       in go (next:ws) rest
-- wow that sucked
