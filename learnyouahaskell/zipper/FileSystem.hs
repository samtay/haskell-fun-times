module FileSystem where

type Name = String
type Data = String
data Item = File Name Data | Folder Name [Item] deriving (Show)
data Crumb = Crumb Name [Item] [Item] deriving (Show)
type Zipper = (Item, [Crumb])

fsUp :: Zipper -> Zipper
fsUp (i, [])  = (i, [])
fsUp (i, Crumb n xs ys:bs) = (Folder n (xs ++ i:ys), bs)

fsTo :: Name -> Zipper -> Zipper
fsTo name (Folder fname fitems, bs) = (item, (Crumb fname ls rs):bs)
  where
    ls   = takeWhile (\i -> nameOf i /= name) fitems
    rs   = drop (length ls + 1) fitems
    item = fitems !! (length ls)

{-- Ah ! Need to learn more of prelude. Learnyouahaskell did me one better
fsTo :: Name -> Zipper -> Zipper
fsTo name (Folder fname fitems, bs) = 
  let (ls, item:rs) = break (\i -> nameOf i == name) fitems
  in (item, Crumb fname ls rs:bs)

-- NOTE break :: (a -> Bool) -> [a] -> ([a], [a])

--}
nameOf :: Item -> Name
nameOf (File n _)   = n
nameOf (Folder n _) = n

fsRename :: Name -> Zipper -> Zipper
fsRename name (File n d, bs) = (File name d, bs)
fsRename name (Folder n xs, bs) = (Folder name xs, bs)

fsTouch :: Item -> Zipper -> Zipper
fsTouch item (Folder n xs, bs) =
  (Folder n (item:xs), bs)

-- dummy data
myDisk :: Item
myDisk = 
  Folder "root" 
    [ File "goat_yelling_like_man.wmv" "baaaaaa"
    , File "pope_time.avi" "god bless"
    , Folder "pics"
      [ File "ape_throwing_up.jpg" "bleargh"
      , File "watermelon_smash.gif" "smash!!"
      , File "skull_man(scary).bmp" "Yikes!"
      ]
    , File "dijon_poupon.doc" "best mustard"
    , Folder "programs"
      [ File "fartwizard.exe" "10gotofart"
      , File "owl_bandit.dmg" "mov eax, h00t"
      , File "not_a_virus.exe" "really not a virus"
      , Folder "source code"
        [ File "best_hs_prog.hs" "main = print (fix error)"
        , File "random.hs" "main = print 4"
        ]
      ]
    ]
