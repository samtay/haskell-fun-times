-- 1. Although it is not possible for a Functor instance to satisfy the first
-- Functor law but not the second (excluding undefined), the reverse is
-- possible. Give an example of a (bogus) Functor instance which satisfies
-- the second law but not the first.

data Pair a = Pair Int a
instance Functor Pair where
  fmap g (Pair x y) = Pair 0 (g y)

-- Now consider any functions g, h where g :: a -> b and h :: a -> b
-- fmap (g . h) (Pair x y) = Pair 0 (g . h) y
--                         = Pair 0 g (h y)
--                         = fmap g (Pair x h y)
--                         = fmap g (fmap h (Pair x y))
--                         = (fmap g) . (fmap h) $ Pair x y
-- Therefore this instance satisfies the second law. However, it clearly doesn't
-- satisfy the first.

-- fmap id (Pair 10 "Oh no!") = Pair 0 "Oh no!" /= Pair 10 "Oh no!"


-- 2. Which laws are violated by the following evil functor instance
instance Functor [] where
  fmap _ [] = []
  fmap g (x:xs) = g x : g x : fmap g xs
-- Both laws. Observe.
-- fmap id [1] = fmap [1, 1] /= [1]
-- fmap ((*2) . (*2)) [1] = [1*2*2, 1*2*2] = [4,4]
-- But when fmap happens twice, the list increases size twice:
-- fmap (*2) . fmap (*2) $ [1] = fmap (*2) $ [1*2, 1*2]
--                             = [1*2*2, 1*2*2, 1*2*2, 1*2*2] = [4,4,4,4] /= [4,4]
