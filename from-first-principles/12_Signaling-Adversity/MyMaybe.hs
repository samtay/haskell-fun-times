module MyMaybe where

-- Small library for Maybe

-- Write the following functions. This may take some time.
-- 1. Simple boolean checks for Maybe values.

-- >>> isJust (Just 1)
-- True
-- >>> isJust Nothing
-- False
isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _       = True

-- >>> isNothing (Just 1)
-- False
-- >>> isNothing Nothing
-- True
isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _       = False

-- 2. The following is the Maybe catamorphism. You can turn a Maybe value into anything else with this.

-- >>> mayybee 0 (+1) Nothing
-- 0
-- >>> mayybee 0 (+1) (Just 1)
-- 2
mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b _ Nothing  = b
mayybee _ f (Just a) = f a

-- 3. In case you just want to provide a fallback value.
-- Try writing it in terms of the maybe catamorphism

-- >>> fromMaybe 0 Nothing
-- 0
-- >>> fromMaybe 0 (Just 1)
-- 1
fromMaybe :: a -> Maybe a -> a
fromMaybe a m = mayybee a id m


-- 4. Converting between List and Maybe.

-- >>> listToMaybe [1, 2, 3]
-- Just 1
-- >>> listToMaybe []
-- Nothing
listToMaybe :: [a] -> Maybe a
listToMaybe []     = Nothing
listToMaybe (x:xs) = Just x

-- >>> maybeToList (Just 1)
-- [1]
-- >>> maybeToList Nothing
-- []
maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just x) = [x]

-- 5. For when we just want to drop the Nothing values from our list.

-- >>> catMaybes [Just 1, Nothing, Just 2]
-- [1, 2]
-- >>> catMaybes [Nothing, Nothing, Nothing]
-- []
catMaybes :: [Maybe a] -> [a]
catMaybes []     = []
catMaybes (x:xs) =  case x of
                   Nothing  -> catMaybes xs
                   (Just a) -> a : catMaybes xs

-- 6. You’ll see this called “sequence” later.

-- >>> flipMaybe [Just 1, Just 2, Just 3]
-- Just [1, 2, 3]
-- >>> flipMaybe [Just 1, Nothing, Just 3]
-- Nothing
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe xs = if hasNothing xs then Nothing else Just (catMaybes xs)
  where hasNothing [] = False
        hasNothing (x:xs) = case x of
                              Nothing   -> True
                              otherwise -> False && hasNothing xs

