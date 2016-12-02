module PoemLines where

-- 2. Split by newlines

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

-- putStrLn sentences -- should print
-- Tyger Tyger, burning bright
-- In the forests of the night
-- What immortal hand or eye
-- Could frame thy fearful symmetry?
-- Implement this
myLines :: String -> [String]
myLines sentences = reverse $ go [] (dropWhile (=='\n') sentences)
  where go ws "" = ws
        go ws s  = let next  = takeWhile (/='\n') s
                       rest = (dropWhile (=='\n')) . (dropWhile (/='\n')) $ s
                       in go (next:ws) rest
-- What we want 'myLines sentences' to equal
shouldEqual =
  [ "Tyger Tyger, burning bright"
  , "In the forests of the night"
  , "What immortal hand or eye"
  , "Could frame thy fearful symmetry?"
  ]
