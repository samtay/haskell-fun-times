import System.Random
import CustomState
randomSt :: (RandomGen g, Random a) => State g a
randomSt = State random

-- improving the old 3 coin toss
threeCoins :: State StdGen (Bool,Bool,Bool)
threeCoins = do
    a <- randomSt
    b <- randomSt
    c <- randomSt
    return (a,b,c)

