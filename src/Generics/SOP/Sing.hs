{-# LANGUAGE PolyKinds, StandaloneDeriving #-}
#if MIN_VERSION_base(4,7,0)
{-# LANGUAGE NoAutoDeriveTypeable #-}
#endif
-- | Singleton types corresponding to type-level data structures.
--
-- The implementation is similar, but subtly different to that of the
-- @<https://hackage.haskell.org/package/singletons singletons>@ package.
-- See the <http://www.andres-loeh.de/TrueSumsOfProducts "True Sums of Products">
-- paper for details.
--
module Generics.SOP.Sing
  ( -- * Singletons
    SList(..)
  , SListI
  , sList
  , para_SList
  , ana_NP_List
    -- ** Shape of type-level lists
  , Shape(..)
  , shape
  , lengthSList
  -- , lengthSing
  ) where

import Data.Proxy

import Generics.SOP.BasicFunctors
import Generics.SOP.Constraint

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
data SList :: [k] -> * where
  SNil  :: SList '[]
  SCons :: SListI xs => SList (x ': xs)

deriving instance Show (SList (xs :: [k]))
deriving instance Eq   (SList (xs :: [k]))
deriving instance Ord  (SList (xs :: [k]))

-- | General eliminator for type-level lists.
--
-- This is a specialization of 'cpara_All' where the constraint
-- is 'Top'.
--
para_SList ::
     SListI xs
  => r '[] -> (forall y ys . SListI ys => r ys -> r (y ': ys))
  -> r xs
para_SList = cpara_All (Proxy :: Proxy Top)

ana_NP_List ::
     SListI xs
  => (forall y ys . s (y ': ys) -> (f y, s ys))
  -> s xs -> NP_List f xs
ana_NP_List = cana_NP_List (Proxy :: Proxy Top)

-- | Get hold of the explicit singleton (for pattern matching).
sList :: SListI xs => SList xs
sList = para_SList SNil (const SCons)

-- | Compute the length of a type-level list.
lengthSList :: forall xs proxy . SListI xs => proxy xs -> Int
lengthSList _ = unK (para_SList (K 0) (\ (K n) -> K (n + 1)) :: K Int xs)

-- * Shape of type-level lists

-- | Occassionally it is useful to have an explicit, term-level, representation
-- of type-level lists (esp because of https://ghc.haskell.org/trac/ghc/ticket/9108)
data Shape :: [k] -> * where
  ShapeNil  :: Shape '[]
  ShapeCons :: SListI xs => Shape xs -> Shape (x ': xs)

deriving instance Show (Shape xs)
deriving instance Eq   (Shape xs)
deriving instance Ord  (Shape xs)

-- | The shape of a type-level list.
shape :: forall (xs :: [k]). SListI xs => Shape xs
shape = case sList :: SList xs of
          SNil  -> ShapeNil
          SCons -> ShapeCons shape
