import Control.Monad
{--
  MonadPlus is for monads that can also act as monoids

class Monad m => MonadPlus m where
  mzero :: m a
  mplus :: m a -> m a -> m a

instance MonadPlus [] where
  mzero = []
  mplus = (++)

guard :: (MonadPlus m) => Bool -> m ()
guard True = return ()
guard False = mzero

--}

-- using binds
luckyNumbers :: [Int] -> [Int]
luckyNumbers xs = xs >>= (\x -> guard ('7' `elem` show x) >> return x)

-- using do
luckyNumbers''' :: [Int] -> [Int]
luckyNumbers''' xs = do
  x <- xs
  guard ('7' `elem` show x)
  return x

-- list comprehension
luckyNumbers'' :: [Int] -> [Int]
luckyNumbers'' xs = [ x | x <- xs, '7' `elem` show x]

-- filter func
luckyNumbers' :: [Int] -> [Int]
luckyNumbers' = filter (elem '7' . show)

