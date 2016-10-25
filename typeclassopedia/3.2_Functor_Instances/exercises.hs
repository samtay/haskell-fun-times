-- 1. Implement Functor instances for Either e and ((->) e)

instance Functor (Either e) where
    fmap g (Left x)  = Left x
    fmap g (Right y) = Right (g y)

instance Functor ((->) e) where
    fmap g f = g . f
--- fmap = (.)


--2. Implement Functor instances for ((,) e) and Pair, where
data Pair a = Pair a a
-- and explain their similarities and differences.

instance Functor ((,) e) where
    fmap g (e, x) = (e, g x)
    
instance Functor (Pair a) where
    fmap g (Pair x y) = Pair (g x) (g y)

-- While these look similar, the difference lies in the kind of the types. Both
-- are type constructors that accept a single concrete type, however ((,) e) has
-- a predefined type (that of e) that is independent of the additional type parameter,
-- while the "first" data constructor parameter of Pair depends on the type parameter.
-- Because of this, I chose to fmap across both elements in the Pair, which makes this
-- instance less useful than ((,) e), since we're essentially just duplicating content.
-- TODO: Make sure we shouldn't implement this as Pair x (g y)
