{-# LANGUAGE PolyKinds #-}
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
type family All (c :: k -> Constraint) (xs :: [k]) :: Constraint
type instance All c '[]       = ()
type instance All c (x ': xs) = (c x, All c xs)

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
type family All2 (c :: k -> Constraint) (xs :: [[k]]) :: Constraint
type instance All2 c '[]       = ()
type instance All2 c (x ': xs) = (All c x, All2 c xs)

-- | A type-level 'map'.
type family Map (f :: k -> l) (xs :: [k]) :: [l]
type instance Map f '[]       = '[]
type instance Map f (x ': xs) = f x ': Map f xs

-- | A generalization of 'All' and 'All2'.
--
-- The family 'AllMap' expands to 'All' or 'All2' depending on whether
-- the argument is indexed by a list or a list of lists.
--
type family AllMap (h :: (k -> *) -> (l -> *)) (c :: k -> Constraint) (xs :: l) :: Constraint

-- | Dictionary for a constraint for all elements of a type-level list.
--
-- A value of type @'AllDict' c xs@ captures the constraint @'All' c xs@.
--
data AllDict (c :: k -> Constraint) (xs :: [k]) where
  AllDictC :: (SingI xs, All c xs) => AllDict c xs
