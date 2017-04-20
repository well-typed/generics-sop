{-# LANGUAGE PolyKinds, UndecidableInstances #-}
#if __GLASGOW_HASKELL__ < 710
{-# LANGUAGE OverlappingInstances #-}
#endif
#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE UndecidableSuperClasses #-}
#endif
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-deprecations #-}
-- | Constraints for indexed datatypes.
--
-- This module contains code that helps to specify that all
-- elements of an indexed structure must satisfy a particular
-- constraint.
--
module Generics.SOP.Constraint
  ( module Generics.SOP.Constraint
  , Constraint
  ) where

import GHC.Exts (Constraint)
import Generics.SOP.Sing

-- | Require a constraint for every element of a list.
--
-- If you have a datatype that is indexed over a type-level
-- list, then you can use 'All' to indicate that all elements
-- of that type-level list must satisfy a given constraint.
--
-- /Example:/ The constraint
--
-- > All Eq '[ Int, Bool, Char ]
--
-- is equivalent to the constraint
--
-- > (Eq Int, Eq Bool, Eq Char)
--
-- /Example:/ A type signature such as
--
-- > f :: All Eq xs => NP I xs -> ...
--
-- means that 'f' can assume that all elements of the n-ary
-- product satisfy 'Eq'.
--
class (AllF c xs, SListI xs) => All (c :: k -> Constraint) (xs :: [k]) where
  -- | Constrained catamorphism for a type-level list.
  --
  -- Strictly speaking, this is even a paramorphism, because we have the
  -- 'All' and therefore 'SListI' constraints available for the tail of
  -- the list in the cons case.
  --
  -- The advantage of writing functions in terms of 'ccataSList' is that
  -- they are typically not recursive, and can be unfolded statically if
  -- the type-level list is statically known.
  --
  ccataSList ::
       proxy c
    -> r '[]
    -> (forall y ys . (c y, All c ys) => r ys -> r (y ': ys))
    -> r xs

  -- | Perform a constrained case distinction on a type-level list.
  ccaseSList ::
       proxy c
    -> r '[]
    -> (forall y ys . (c y, All c ys) => r (y ': ys))
    -> r xs
  ccaseSList p nil cons = ccataSList p nil (const cons)
  {-# INLINE ccaseSList #-}

instance All c '[] where
  ccataSList _p nil _cons = nil
  {-# INLINE ccataSList #-}

instance (c x, All c xs) => All c (x ': xs) where
  ccataSList p nil cons =
    cons (ccataSList p nil cons)
  {-# INLINE ccataSList #-}

-- | Type family used to implement 'All'.
--
type family AllF (c :: k -> Constraint) (xs :: [k]) :: Constraint
type instance AllF _c '[]       = ()
type instance AllF  c (x ': xs) = (c x, All c xs)

-- | Require a singleton for every inner list in a list of lists.
type SListI2 = All SListI

-- | Require a constraint for every element of a list of lists.
--
-- If you have a datatype that is indexed over a type-level
-- list of lists, then you can use 'All2' to indicate that all
-- elements of the innert lists must satisfy a given constraint.
--
-- /Example:/ The constraint
--
-- > All2 Eq '[ '[ Int ], '[ Bool, Char ] ]
--
-- is equivalent to the constraint
--
-- > (Eq Int, Eq Bool, Eq Char)
--
-- /Example:/ A type signature such as
--
-- > f :: All2 Eq xss => SOP I xs -> ...
--
-- means that 'f' can assume that all elements of the sum
-- of product satisfy 'Eq'.
--
class (All (All f) xss, All SListI xss) => All2 f xss
instance (All (All f) xss, All SListI xss) => All2 f xss
--
-- NOTE:
--
-- The definition
--
-- type All2 f = All (All f)
--
-- is more direct, but has the unfortunate disadvantage the
-- it triggers GHC's superclass cycle check when used in a
-- class context.

-- | Composition of constraints.
--
-- Note that the result of the composition must be a constraint,
-- and therefore, in @f ':.' g@, the kind of @f@ is @k -> 'Constraint'@.
-- The kind of @g@, however, is @l -> k@ and can thus be an normal
-- type constructor.
--
-- A typical use case is in connection with 'All' on an 'NP' or an
-- 'NS'. For example, in order to denote that all elements on an
-- @'NP' f xs@ satisfy 'Show', we can say @'All' ('Show' :. f) xs@.
--
-- @since 0.2
--
class (f (g x)) => (f `Compose` g) x
instance (f (g x)) => (f `Compose` g) x
infixr 9 `Compose`

-- | Pairing of constraints.
--
-- @since 0.2
--
class (f x, g x) => (f `And` g) x
instance (f x, g x) => (f `And` g) x
infixl 7 `And`

-- | A constraint that can always be satisfied.
--
-- @since 0.2
--
class Top x
instance Top x

-- | A generalization of 'All' and 'All2'.
--
-- The family 'AllN' expands to 'All' or 'All2' depending on whether
-- the argument is indexed by a list or a list of lists.
--
type family AllN (h :: (k -> *) -> (l -> *)) (c :: k -> Constraint) :: l -> Constraint

-- | A generalization of 'SListI'.
--
-- The family 'SListIN' expands to 'SListI' or 'SListI2' depending
-- on whether the argument is indexed by a list or a list of lists.
--
type family SListIN (h :: (k -> *) -> (l -> *)) :: l -> Constraint

instance
#if __GLASGOW_HASKELL__ >= 710
  {-# OVERLAPPABLE #-}
#endif
  SListI xs => SingI (xs :: [k]) where
  sing = sList

instance
#if __GLASGOW_HASKELL__ >= 710
  {-# OVERLAPPING #-}
#endif
  (All SListI xss, SListI xss) => SingI (xss :: [[k]]) where
  sing = sList
