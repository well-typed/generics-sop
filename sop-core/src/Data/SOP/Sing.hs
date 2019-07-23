{-# LANGUAGE PolyKinds, StandaloneDeriving #-}
#if __GLASGOW_HASKELL__ < 806
-- Before GHC 8.6, TypeInType was required to explicitly quantify kind variables.
-- After GHC 8.6, this feature was incorporated into PolyKinds.
{-# LANGUAGE TypeInType #-}
#endif
-- | Singleton types corresponding to type-level data structures.
--
-- The implementation is similar, but subtly different to that of the
-- @<https://hackage.haskell.org/package/singletons singletons>@ package.
-- See the <http://www.andres-loeh.de/TrueSumsOfProducts "True Sums of Products">
-- paper for details.
--
module Data.SOP.Sing
  ( -- * Singletons
    SList(..)
  , SListI
  , sList
  , para_SList
  , case_SList
    -- ** Shape of type-level lists
  , Shape(..)
  , shape
  , lengthSList
  ) where

import Data.Kind (Type)
import Data.Proxy (Proxy(..))

import Data.SOP.Constraint

-- * Singletons

-- | Explicit singleton list.
--
-- A singleton list can be used to reveal the structure of
-- a type-level list argument that the function is quantified
-- over. For every type-level list @xs@, there is one non-bottom
-- value of type @'SList' xs@.
--
-- Note that these singleton lists are polymorphic in the
-- list elements; we do not require a singleton representation
-- for them.
--
-- @since 0.2
--
data SList :: [k] -> Type where
  SNil  :: SList '[]
  SCons :: SListI xs => SList (x ': xs)

deriving instance Show (SList (xs :: [k]))
deriving instance Eq   (SList (xs :: [k]))
deriving instance Ord  (SList (xs :: [k]))

-- | Paramorphism for a type-level list.
--
-- @since 0.4.0.0
--
para_SList ::
     SListI xs
  => r '[]
  -> (forall y ys . (SListI ys) => r ys -> r (y ': ys))
  -> r xs
para_SList nil cons =
  cpara_SList (Proxy :: Proxy Top) nil cons
{-# INLINE para_SList #-}

-- | Case distinction on a type-level list.
--
-- @since 0.4.0.0
--
case_SList ::
     SListI xs
  => r '[]
  -> (forall y ys . (SListI ys) => r (y ': ys))
  -> r xs
case_SList nil cons =
  ccase_SList (Proxy :: Proxy Top) nil cons
{-# INLINE case_SList #-}

-- | Get hold of an explicit singleton (that one can then
-- pattern match on) for a type-level list
--
sList :: SListI xs => SList xs
sList = ccase_SList (Proxy :: Proxy Top) SNil SCons

-- * Shape of type-level lists

-- | Occasionally it is useful to have an explicit, term-level, representation
-- of type-level lists (esp because of https://ghc.haskell.org/trac/ghc/ticket/9108 )
--
data Shape :: [k] -> Type where
  ShapeNil  :: Shape '[]
  ShapeCons :: SListI xs => Shape xs -> Shape (x ': xs)

deriving instance Show (Shape xs)
deriving instance Eq   (Shape xs)
deriving instance Ord  (Shape xs)

-- | The shape of a type-level list.
shape :: forall k (xs :: [k]). SListI xs => Shape xs
shape = case sList :: SList xs of
          SNil  -> ShapeNil
          SCons -> ShapeCons shape

-- | The length of a type-level list.
--
-- @since 0.2
--
lengthSList :: forall k (xs :: [k]) proxy. SListI xs => proxy xs -> Int
lengthSList _ = lengthShape (shape :: Shape xs)
  where
    lengthShape :: forall xs'. Shape xs' -> Int
    lengthShape ShapeNil      = 0
    lengthShape (ShapeCons s) = 1 + lengthShape s
