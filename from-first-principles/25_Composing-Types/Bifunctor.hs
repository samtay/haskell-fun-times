module Bifunctor where

class Bifunctor p where
  {-# MINIMAL bimap | first, second #-}

  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
  bimap f g = first f . second g

  first :: (a -> b) -> p a c -> p b c
  first f = bimap f id

  second :: (b -> c) -> p a b -> p a c
  second = bimap id

-------------------------------------------------------------------------------
------------------------------          1          ----------------------------
-------------------------------------------------------------------------------

data Deux a b = Deux a b

instance Bifunctor Deux where
  bimap f g (Deux a c) = Deux (f a) (g c)

-------------------------------------------------------------------------------
------------------------------          2          ----------------------------
-------------------------------------------------------------------------------

data Const a b = Const a

instance Bifunctor Const where
  bimap f _ (Const a) = Const (f a)

-------------------------------------------------------------------------------
------------------------------          3          ----------------------------
-------------------------------------------------------------------------------

data Drei a b c = Drei a b c

instance Bifunctor (Drei z) where
  bimap f g (Drei z a b) = Drei z (f a) (g b)

-------------------------------------------------------------------------------
------------------------------          4          ----------------------------
-------------------------------------------------------------------------------

data SuperDrei a b c = SuperDrei a b

instance Bifunctor (SuperDrei a) where
  bimap bd _ (SuperDrei a b) =
    SuperDrei a (bd b)

-------------------------------------------------------------------------------
------------------------------          5          ----------------------------
-------------------------------------------------------------------------------

data SemiDrei a b c = SemiDrei a

instance Bifunctor (SemiDrei a) where
  bimap _ _ (SemiDrei a) = (SemiDrei a)
  --
  -- Ahhh.. id isn't allowed because it enforces id :: a -> a,
  -- and while the "value" is the "same", really the type is changing. Interesting.
  -- bimap _ _ = id
  --
  -- Oddly, below doesn't work either o_O
  -- bimap _ _ x = x

-------------------------------------------------------------------------------
------------------------------          6          ----------------------------
-------------------------------------------------------------------------------

data Quadriceps a b c d = Quadzzz a b c d

instance Bifunctor (Quadriceps a b) where
  bimap f g (Quadzzz a b c d) =
    Quadzzz a b (f c) (g d)

-------------------------------------------------------------------------------
------------------------------          7          ----------------------------
-------------------------------------------------------------------------------

instance Bifunctor Either where
  bimap f _ (Left a)  = Left (f a)
  bimap _ g (Right b) = Right (g b)
