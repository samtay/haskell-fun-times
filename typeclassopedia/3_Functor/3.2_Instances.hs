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

instance Functor Pair where
  fmap g (Pair x y) = Pair (g x) (g y)

-- While these look similar, the difference lies in the kind of the types. Both
-- are type constructors that accept a single concrete type, however ((,) e) has
-- a predefined type (that of e) that is independent of the additional type parameter,
-- while the "first" data constructor parameter of Pair depends on the type parameter.
-- Because of this, I chose to fmap across both elements in the Pair, which makes this
-- instance less useful than ((,) e), since we're essentially just duplicating content.
-- TODO: Make sure we shouldn't implement this as Pair x (g y)


-- 3. Implement a Functor instance for the type ITree defined as
data ITree a = Leaf (Int -> a)
             | Node [ITree a]
instance Functor ITree where
  fmap g (Leaf h)  = Leaf (g . h)
  fmap g (Node xs) = Node $ map (fmap g) xs

-- 4. Give an example of a type of kind * -> * which cannot be made an instance
-- of Functor (without using undefined).

data Jumble a = Jumble (a -> String)

-- Jumble cannot be made an instance of Functor because fmap :: (a -> b) -> f a -> f b.
-- But in this case, notice that f a === Jumble (a -> String). So what would we do with
-- the inital argument that is a function of type (a -> b) ? We need to end up with a
-- type of Jumble (b -> String), in other words a function that accepts type b. There's
-- no way for us to do this because we aren't guaranteed an inverse of any of these functions.
-- NOTE this is like the opposite of ((->) b), where the type parameter on which Functor
-- operates is the SECOND parameter.
-- TODO: Come up with a more coherent explanation of why Functor cannot be implemented for Jumble


-- 5. Is this statement true or false?
-- >> The composition of two Functors is also a Functor

{-- I believe this is true.

Proof: Consider types A and B which are each of kind * -> *. Without loss of generality,
let us consider the composed type C = B A, which is also of kind * -> *. Since A and B
are each Functors, we know that for any x,y of indeterminate type,
fmap f (B x) = B (f x)
fmap f (A y) = A (f y)
Therefore we can define fmap for type C as
fmap f (C z) =
fmap f (B (A z)) = let (A y) = fmap f (A z)
                   in (A y) <$ (B z) -- does second parameter matter?
Notice we only need to fmap f once, to transform (a -> b). The second usage of the Functor class
is just <$ which is fmap . const, and is just used to take the already fmapped value (in context of A)
and minimally put it into the context of B.
Note: This proof sucks.
--}
