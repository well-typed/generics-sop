{-# LANGUAGE PolyKinds, UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-deprecations #-}
-- | Constraints for indexed datatypes.
--
-- This module contains code that helps to specify that all
-- elements of an indexed structure must satisfy a particular
-- constraint.
--
module Data.SOP.Constraint
  ( module Data.SOP.Constraint
  , Constraint
  ) where

import Data.Coerce
import Data.Kind (Type, Constraint)

-- import Data.SOP.Sing

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
-- Note on superclasses: ghc cannot deduce superclasses from 'All'
-- constraints.
-- You might expect the following to compile
--
-- > class (Eq a) => MyClass a
-- >
-- > foo :: (All Eq xs) => NP f xs -> z
-- > foo = [..]
-- >
-- > bar :: (All MyClass xs) => NP f xs -> x
-- > bar = foo
-- but it will fail with an error saying that it was unable to
-- deduce the class constraint @'AllF' 'Eq' xs@ (or similar) in the
-- definition of 'bar'.
-- In cases like this you can use 'Data.SOP.Dict.Dict' from "Data.SOP.Dict"
-- to prove conversions between constraints.
-- See [this answer on SO for more details](https://stackoverflow.com/questions/50777865/super-classes-with-all-from-generics-sop).

--
class (AllF c xs, SListI xs) => All (c :: k -> Constraint) (xs :: [k]) where

  -- | Constrained paramorphism for a type-level list.
  --
  -- The advantage of writing functions in terms of 'cpara_SList' is that
  -- they are then typically not recursive, and can be unfolded statically if
  -- the type-level list is statically known.
  --
  -- @since 0.4.0.0
  --
  cpara_SList ::
       proxy c
    -> r '[]
    -> (forall y ys . (c y, All c ys) => r ys -> r (y ': ys))
    -> r xs

  -- | Constrained case distinction on a type-level list.
  --
  -- @since 0.4.0.0
  --
  ccase_SList ::
       All c xs
    => proxy c
    -> ((xs ~ '[]) => r '[])
    -> (forall y ys . (c y, All c ys, (y ': ys) ~ xs) => r (y ': ys))
    -> r xs

instance All c '[] where
  cpara_SList _p nil _cons = nil
  {-# INLINE cpara_SList #-}

  ccase_SList _p nil _cons =
    nil
  {-# INLINE ccase_SList #-}

instance (c x, All c xs) => All c (x ': xs) where
  cpara_SList p nil cons =
    cons (cpara_SList p nil cons)
  {-# INLINE cpara_SList #-}

  ccase_SList _p _nil cons =
    cons
  {-# INLINE ccase_SList #-}

-- | Type family used to implement 'All'.
--
type family
  AllF (c :: k -> Constraint) (xs :: [k]) :: Constraint where
  AllF _c '[]       = ()
  AllF  c (x ': xs) = (c x, All c xs)

-- | Require a singleton for every inner list in a list of lists.
type SListI2 = All SListI

-- | Implicit singleton list.
--
-- A singleton list can be used to reveal the structure of
-- a type-level list argument that the function is quantified
-- over.
--
-- Since 0.4.0.0, this is now defined in terms of 'All'.
-- A singleton list provides a witness for a type-level list
-- where the elements need not satisfy any additional
-- constraints.
--
-- @since 0.4.0.0
--
type SListI = All Top

-- | Require a constraint for every element of a list of lists.
--
-- If you have a datatype that is indexed over a type-level
-- list of lists, then you can use 'All2' to indicate that all
-- elements of the inner lists must satisfy a given constraint.
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
-- Since 0.4.0.0, this is merely a synonym for
-- 'All (All c)'.
--
-- @since 0.4.0.0
--
type All2 c = All (All c)

-- | Require a constraint pointwise for every pair of
-- elements from two lists.
--
-- /Example:/ The constraint
--
-- > AllZip (~) '[ Int, Bool, Char ] '[ a, b, c ]
--
-- is equivalent to the constraint
--
-- > (Int ~ a, Bool ~ b, Char ~ c)
--
-- @since 0.3.1.0
--
class
  ( SListI xs, SListI ys
  , SameShapeAs xs ys, SameShapeAs ys xs
  , AllZipF c xs ys
  ) => AllZip (c :: a -> b -> Constraint) (xs :: [a]) (ys :: [b])
instance
  ( SListI xs, SListI ys
  , SameShapeAs xs ys, SameShapeAs ys xs
  , AllZipF c xs ys
  ) => AllZip c xs ys

-- | Type family used to implement 'AllZip'.
--
-- @since 0.3.1.0
--
type family
  AllZipF (c :: a -> b -> Constraint) (xs :: [a]) (ys :: [b])
    :: Constraint where
  AllZipF _c '[]      '[]        = ()
  AllZipF  c (x ': xs) (y ': ys) = (c x y, AllZip c xs ys)

-- | Type family that forces a type-level list to be of the same
-- shape as the given type-level list.
--
-- Since 0.5.0.0, this only tests the top-level structure of
-- the list, and is intended to be used in conjunction with
-- a separate construct (such as the 'AllZip', 'AllZipF'
-- combination to tie the recursive knot). The reason is that
-- making 'SameShapeAs' directly recursive leads to quadratic
-- compile times.
--
-- The main use of this constraint is to help type inference to
-- learn something about otherwise unknown type-level lists.
--
-- @since 0.5.0.0
--
type family
  SameShapeAs (xs :: [a]) (ys :: [b]) :: Constraint where
  SameShapeAs '[]       ys = (ys ~ '[])
  SameShapeAs (x ': xs) ys = (ys ~ (Head ys ': Tail ys))

-- | Utility function to compute the head of a type-level list.
--
-- @since 0.3.1.0
--
type family Head (xs :: [a]) :: a where
  Head (x ': xs) = x

-- | Utility function to compute the tail of a type-level list.
--
-- @since 0.3.1.0
--
type family Tail (xs :: [a]) :: [a] where
  Tail (x ': xs) = xs

-- | The constraint @'LiftedCoercible' f g x y@ is equivalent
-- to @'Data.Coerce.Coercible' (f x) (g y)@.
--
-- @since 0.3.1.0
--
class Coercible (f x) (g y) => LiftedCoercible f g x y
instance Coercible (f x) (g y) => LiftedCoercible f g x y

-- | Require a constraint pointwise for every pair of
-- elements from two lists of lists.
--
--
class (AllZipF (AllZip f) xss yss, SListI xss, SListI yss, SameShapeAs xss yss, SameShapeAs yss xss) => AllZip2 f xss yss
instance (AllZipF (AllZip f) xss yss, SListI xss, SListI yss, SameShapeAs xss yss, SameShapeAs yss xss) => AllZip2 f xss yss

-- | Composition of constraints.
--
-- Note that the result of the composition must be a constraint,
-- and therefore, in @'Compose' f g@, the kind of @f@ is @k -> 'Constraint'@.
-- The kind of @g@, however, is @l -> k@ and can thus be a normal
-- type constructor.
--
-- A typical use case is in connection with 'All' on an 'Data.SOP.NP' or an
-- 'Data.SOP.NS'. For example, in order to denote that all elements on an
-- @'Data.SOP.NP' f xs@ satisfy 'Show', we can say @'All' ('Compose' 'Show' f) xs@.
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
type family AllN (h :: (k -> Type) -> (l -> Type)) (c :: k -> Constraint) :: l -> Constraint

-- | A generalization of 'AllZip' and 'AllZip2'.
--
-- The family 'AllZipN' expands to 'AllZip' or 'AllZip2' depending on
-- whther the argument is indexed by a list or a list of lists.
--
type family AllZipN (h :: (k -> Type) -> (l -> Type)) (c :: k1 -> k2 -> Constraint) :: l1 -> l2 -> Constraint

-- | A generalization of 'SListI'.
--
-- The family 'SListIN' expands to 'SListI' or 'SListI2' depending
-- on whether the argument is indexed by a list or a list of lists.
--
type family SListIN (h :: (k -> Type) -> (l -> Type)) :: l -> Constraint
