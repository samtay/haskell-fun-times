module Phone where

import Data.Char
import Data.List

-- ---------------------------
-- | 1      | 2 ABC | 3 DEF  |
-- ---------------------------
-- | 4 GHI  | 5 JKL | 6 MNO  |
-- ---------------------------
-- | 7 PQRS | 8 TUV | 9 WXYZ |
-- ---------------------------
-- | * ^    | 0 + _ | # .,   |
-- ---------------------------

-- 2 -> 'A'
-- 22 -> 'B'
-- 222 -> 'C'
-- 2222 -> '2'
-- 22222 -> 'A'

-- 1. Create a data structure that captures the phone layout above.
-- wtf do I need this for
data DaPhone = K1 | K2 | K3
             | K4 | K5 | K6
             | K7 | K8 | K9
             | Ks | K0 | Kp

phone :: [(Char, [Char])]
phone = [ ('1', "1")
        , ('2', "abc2")
        , ('3', "def3")
        , ('4', "ghi4")
        , ('5', "jkl5")
        , ('6', "mno6")
        , ('7', "pqrs7")
        , ('8', "tuv8")
        , ('9', "wxyz9")
        , ('0', " 0")
        , ('#', ".,")
        ]

letterToDigitMap :: Char -> Maybe Char
letterToDigitMap c = lookup (toLower c) l2dMap
  where l2dMap = [(x,y) | (y, xs) <- phone, x <- xs]

-- 2. Convert the following conversations into the keypresses required to express them.
convo :: [String]
convo =
  ["Wanna play 20 questions",
  "Ya",
  "U 1st haha",
  "Lol ok. Have u ever tasted alcohol lol",
  "Lol ya",
  "Wow ur cool haha. Ur turn",
  "Ok. Do u think I am pretty Lol",
  "Lol ya",
  "Haha thanks just making sure rofl ur turn"]

-- validButtons = "1234567890*#"
type Digit = Char
-- Valid presses: 1 and up
type Presses = Int

--reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps :: Char -> [(Digit, Presses)]
reverseTaps c = if shifted then ('*', 1):[(d, n+1)] else [(d,n+1)]
  where (Just d) = letterToDigitMap c
        shifted  = isUpper c
        (Just n) = let (Just cs) = lookup d phone
                    in elemIndex (toLower c) cs

translateMsg :: String -> [(Digit, Presses)]
translateMsg = concatMap reverseTaps

translateConvo :: [String] -> [[(Digit, Presses)]]
translateConvo = map translateMsg

-- 3. How many times do digits need to be pressed for each message?
fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = foldr (\(d,p) acc -> p + acc) 0


-- 4. What was the most popular letter for each message? What was its cost? You’ll want to combine reverseTaps and fingerTaps to figure out what it cost in taps. reverseTaps is a list because you need to press a different button in order to get capitals.
mostPopularLetter :: String -> Char
mostPopularLetter msg = foldr (\c m -> let cn      = count c msg
                                           mn      = count m msg
                                        in if cn > mn then c else m
                              ) (head msg) (nub msg)

count :: (Eq a) => a -> [a] -> Int
count x = length . filter (x==)


-- 5. What was the most popular letter overall? What was the most popular word?
coolestLtr :: [String] -> Char
coolestLtr convo = mostPopularLetter $ concat convo

-- notice that mostPopularLetter is not char specific
mostPopularX :: (Eq a) => [a] -> a
mostPopularX [] = error "not good error"
mostPopularX xs = foldr (\x m -> let xn = count x xs
                                     mn = count m xs
                                  in if xn > mn then x else m
                              ) (head xs) (nub xs)

coolestWord :: [String] -> String
coolestWord = mostPopularX . concatMap words
