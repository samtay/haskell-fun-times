import qualified BinaryTree as T
import Data.Monoid

instance Foldable T.Tree where
  foldMap f T.EmptyTree = mempty
  foldMap f (T.Node x l r) = foldMap f l `mappend`
                           f x         `mappend`
                           foldMap f r


addEmUp :: (Num a) => T.Tree a -> a
addEmUp = foldl (+) 0

-- improved from T
elemTree :: (Eq a) => a -> T.Tree a -> Bool
elemTree x = getAny . foldMap (\y -> Any $ x == y)

toArr :: T.Tree a -> [a]
toArr = foldMap (\x -> [x])
