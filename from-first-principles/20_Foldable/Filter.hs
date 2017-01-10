module Filter where

-- | Folds while filtering via specified predicate
--
-- Still not sure why we need applicative, instead of just
-- filterF:: (Foldable t, Monoid (t a)) => (a -> Bool) -> t a -> t a
filterF :: (Applicative f, Foldable t, Monoid (f a))
        => (a -> Bool) -> t a -> f a
filterF p = foldMap (\x -> if p x then pure x else mempty)
