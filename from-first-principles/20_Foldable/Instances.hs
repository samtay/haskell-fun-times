module Instances where

import Data.Monoid ((<>))

-- 1
data Constant a b = Constant a

instance Foldable (Constant a) where
  foldr _ z _ = z
  -- or --
  foldMap _ _ = mempty

-- 2
data Two a b = Two a b
instance Foldable (Two a) where
  foldr f z (Two _ x) = f x z
  -- or --
  foldMap f (Two _ x) = f x

-- 3
data Three a b c = Three a b c
instance Foldable (Three a b) where
  foldr f z (Three _ _ x) = f x z
  -- or --
  foldMap f (Three _ _ x) = f x

-- 4
data Three' a b = Three' a b b
instance Foldable (Three' a) where
  foldr f z (Three' _ x y) = f x (f y z)
  -- or --
  foldMap f (Three' _ x y) = f x <> f y

-- 5
data Four' a b = Four' a b b b
instance Foldable (Four' a) where
  foldr f z (Four' _ w x y) = f w $ f x $ f y z
  -- or --
  foldMap f (Four' _ w x y) = f w <> f x <> f y
  -- or --
  -- foldMap f (Four' _ w x y) = mconcat $ f <$> [w,x,y]
