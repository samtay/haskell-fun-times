module Knight where
import Control.Monad

type KnightPos = (Int,Int)

-- using guard in do block
possibleMoves :: KnightPos -> [KnightPos]
possibleMoves (c,r) = do
  (c',r') <- [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)
             ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)
             ]
  guard (c' `elem` [1..8] && r' `elem` [1..8])
  return (c',r')

-- monad lists really allow easy syntax for nondeterminism
in3 :: KnightPos -> [KnightPos]
in3 start = do
  first <- possibleMoves start
  second <- possibleMoves first
  possibleMoves second

-- using >>= notation
in3' :: KnightPos -> [KnightPos]
in3' start = return start >>= possibleMoves >>= possibleMoves >>= possibleMoves

-- but how can we make the "3" dynamic??? hmmm
