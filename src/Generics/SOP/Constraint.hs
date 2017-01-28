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

import Data.Proxy
import GHC.Exts (Any, Constraint)
import Generics.SOP.BasicFunctors
import Unsafe.Coerce

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
class (SListI xs, AllF c xs) => All (c :: k -> Constraint) (xs :: [k]) where

  -- | General constrained eliminator for type-level lists.
  --
  -- Takes an argument for the case of the empty list, and an argument
  -- for the case of the non-empty list. The non-empty case can make use
  -- of the constraint for the head, and of an 'All' constraint on the
  -- tail.
  --
  cpara_All ::
       Proxy c
    -> r '[]
    -> (forall y ys . (c y, All c ys) => r ys -> r (y ': ys))
    -> r xs

instance All c '[] where
  cpara_All _ nil _cons = nil
  {-# INLINE cpara_All #-}

instance (c x, All c xs) => All c (x ': xs) where
  cpara_All p nil cons = cons (cpara_All p nil cons)
  {-# INLINE cpara_All #-}

newtype NP_List f xs = NP_List { unNP_List :: [f Any] }

nil_NP_List :: NP_List f '[]
nil_NP_List = NP_List []

cons_NP_List :: f x -> NP_List f xs -> NP_List f (x ': xs)
cons_NP_List x (NP_List xs) = NP_List (unsafeCoerce x : xs)
{-# INLINE cons_NP_List #-}

cana_NP_List ::
     All c xs
  => Proxy c
  -> (forall y ys . c y => s (y ': ys) -> (f y, s ys))
  -> s xs -> NP_List f xs
cana_NP_List p uncons =
  apFn $ cpara_All p
    (Fn $ \ _ -> NP_List [])
    (\ rec -> Fn $ \ s -> case uncons s of
      (x, s') -> cons_NP_List x (apFn rec s'))
{-# INLINE cana_NP_List #-}

cpure_NP_List :: All c xs => Proxy c -> (forall a . c a => f a) -> NP_List f xs
cpure_NP_List p x = cana_NP_List p (\ _ -> (x, K ())) (K ())
{-# INLINE cpure_NP_List #-}

-- | Type family used to implement 'All'.
--
type family AllF (c :: k -> Constraint) (xs :: [k]) :: Constraint
type instance AllF _c '[]       = ()
type instance AllF  c (x ': xs) = (c x, All c xs)

-- | Implicit singleton list.
--
-- A singleton list can be used to reveal the structure of
-- a type-level list argument that the function is quantified
-- over.
--
-- @since 0.2
--
type SListI = All Top

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
type All2 f = All (All f)

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

