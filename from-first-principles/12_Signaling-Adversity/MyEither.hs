module MyEither where

--Small library for Either
-- Write each of the following functions. If more than one possible unique function exists for the type, use common sense to determine what it should do.

-- 1. Try to eventually arrive at a solution that uses foldr, even if earlier versions don’t use foldr.

lefts' :: [Either a b] -> [a]
lefts' []            = []
lefts' ((Left a):xs) = a : lefts' xs
lefts' (_:xs)        = lefts' xs

lefts'' :: [Either a b] -> [a]
lefts'' = foldr go []
  where go (Left a) xs = a : xs
        go _ xs        = xs

rights' :: [Either a b] -> [b]
rights' = foldr go []
  where go (Right a) xs = a : xs
        go _ xs        = xs

-- 3.

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' es = (lefts'' es, rights' es)

-- 4.

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left a)  = Nothing
eitherMaybe' f (Right b) = Just $ f b

-- 5.  This is a general catamorphism for Either values.

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left a)  = f a
either' _ g (Right b) = g b

-- 6. Same as before, but use the either' function you just wrote.

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (\_ -> Nothing) (Just . f)
