{-# LANGUAGE PolyKinds, NoAutoDeriveTypeable #-}
-- | Singleton types corresponding to type-level data structures.
--
-- The implementation is similar, but subtly different to that of the
-- singletons package. See the "True Sums of Products" paper for
-- details.
--
module Generics.SOP.Sing where

import Data.Proxy (Proxy(..))

-- | Explicit singleton.
--
-- A singleton can be used to reveal the structure of a type
-- argument that the function is quantified over.
--
-- The family 'Sing' should have at most one instance per kind,
-- and there should be a matching instance for 'SingI'.
--
data family Sing (a :: k)

-- | Singleton for type-level lists.
data instance Sing (xs :: [k]) where
  SNil  :: Sing '[]
  SCons :: (SingI x, SingI xs) => Sing (x ': xs)

-- | Singleton for types of kind '*'.
--
-- For types of kind '*', we explicitly *don't* want to reveal
-- more type analysis. Even functions that have a 'Sing' constraint
-- should still be parametric in everything that is of kind '*'.
--
data instance Sing (x :: *) where
  SStar :: Sing (x :: *)

-- | Implicit singleton.
--
-- A singleton can be used to reveal the structure of a type
-- argument that the function is quantified over.
--
-- The class 'SingI' should have instances that match the
-- family instances for 'Sing'.
--
class SingI (a :: k) where
  -- | Get hold of the explicit singleton (that one can then
  -- pattern match on).
  sing :: Sing a

instance SingI (x :: *) where
  sing = SStar

instance SingI '[] where
  sing = SNil

instance (SingI x, SingI xs) => SingI (x ': xs) where
  sing = SCons

{-------------------------------------------------------------------------------
  Shape of type-level lists
-------------------------------------------------------------------------------}

-- | Occassionally it is useful to have an explicit, term-level, representation
-- of type-level lists (esp because of https://ghc.haskell.org/trac/ghc/ticket/9108)
data Shape :: [k] -> * where
  ShapeNil  :: Shape '[]
  ShapeCons :: SingI xs => Shape xs -> Shape (x ': xs)

shape :: forall (xs :: [k]). SingI xs => Shape xs
shape = case sing :: Sing xs of
          SNil  -> ShapeNil
          SCons -> ShapeCons shape

lengthSing :: forall (xs :: [k]). SingI xs => Proxy xs -> Int
lengthSing _ = lengthShape (shape :: Shape xs)
  where
    lengthShape :: forall xs'. Shape xs' -> Int
    lengthShape ShapeNil      = 0
    lengthShape (ShapeCons s) = 1 + lengthShape s
