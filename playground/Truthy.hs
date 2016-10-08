{--
  parallel to (Eq) or (Ord) type classes, type instances of (Truthy)
  will support usage of (truthy) function
--}
import TrafficLight
import BinaryTree

class Truthy a where
  truthy :: a -> Bool


instance Truthy Int where
  truthy 0 = False
  truthy _ = True

instance Truthy [a] where
  truthy [] = False
  truthy _ = True

instance Truthy Bool where
  truthy = id -- note the implicit use of currying

instance Truthy (Maybe a) where
  truthy (Just _) = True
  truthy Nothing = False

instance Truthy (Tree a) where
  truthy EmptyTree = False
  truthy _ = True

instance Truthy TrafficLight where
  truthy Red = False
  truthy _ = True
