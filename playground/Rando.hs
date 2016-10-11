import System.Random

main = do
  gen <- getStdGen
  putStrLn $ show $ let (n, newGen) = random gen in rollDie n
  putStrLn $ take 20 (randomRs ('a', 'z') gen)

-- three random coin flips
-- note Haskell inferring that random gen* :: (Bool, StdGen)
threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen =
  let (firstCoin, gen1) = random gen
      (secondCoin, gen2) = random gen1
      (thirdCoin, _) = random gen2
   in (firstCoin, secondCoin, thirdCoin)

-- randoms constructs infinite list
{-- usage:
  nRandoms 10 10 :: [Float]
--}
nRandoms :: (Random a) => Int -> Int -> [a]
nRandoms n starter = take n $ randoms (mkStdGen starter)

rollDie :: Int -> Int
rollDie starter = let (roll, _) = randomR (1,6) (mkStdGen starter) in roll

randomLetters :: Int -> Int -> String
randomLetters n starter = take n $ randomRs ('a', 'z') (mkStdGen starter) :: String
