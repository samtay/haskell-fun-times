import qualified Data.Map as Map

data LockerState = Taken | Free deriving (Show, Eq) -- two states for locker
type Code = String
type LockerMap = Map.Map Int (LockerState, Code) -- [ int -> [state, code] ]

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup num map =
  case Map.lookup num map of
    Nothing -> Left $ "Locker number " ++ show num ++ " doesn't exist!"
    Just (state, code) -> if state /= Taken
                             then Right code
                             else Left $ "Locker number " ++ show num ++ " is taken!"


-- data
lockers :: LockerMap
lockers = Map.fromList
    [(100,(Taken,"ZD39I"))
    ,(101,(Free,"JAH3I"))
    ,(103,(Free,"IQSA9"))
    ,(105,(Free,"QOTSA"))
    ,(109,(Taken,"893JJ"))
    ,(110,(Taken,"99292"))
    ]
