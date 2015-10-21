{-# LANGUAGE PolyKinds, UndecidableInstances #-}
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
class (AllF f xs, SListI xs) => All (f :: k -> Constraint) (xs :: [k])
instance (AllF f xs, SListI xs) => All f xs

-- | Type family used to implement 'All'.
--
type family AllF (c :: k -> Constraint) (xs :: [k]) :: Constraint
type instance AllF c '[]       = ()
type instance AllF c (x ': xs) = (c x, All c xs)

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
class (f (g x)) => (f :. g) x
instance (f (g x)) => (f :. g) x
infixr 9 :.

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
