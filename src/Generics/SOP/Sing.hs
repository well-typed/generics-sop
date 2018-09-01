{-# LANGUAGE PolyKinds, StandaloneDeriving #-}
#if MIN_VERSION_base(4,7,0)
{-# LANGUAGE NoAutoDeriveTypeable #-}
#endif
#if __GLASGOW_HASKELL__ >= 805
{-# LANGUAGE StarIsType #-}
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
  , SListI(..)
  , Sing
  , SingI(..)
    -- ** Shape of type-level lists
  , Shape(..)
  , shape
  , lengthSList
  , lengthSing
  ) where

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

-- | Implicit singleton list.
--
-- A singleton list can be used to reveal the structure of
-- a type-level list argument that the function is quantified
-- over.
--
-- The class 'SListI' should have instances that match the
-- constructors of 'SList'.
--
-- @since 0.2
--
class SListI (xs :: [k]) where
  -- | Get hold of the explicit singleton (that one can then
  -- pattern match on).
  sList :: SList xs

instance SListI '[] where
  sList = SNil

instance SListI xs => SListI (x ': xs) where
  sList = SCons

-- | General class for implicit singletons.
--
-- Just provided for limited backward compatibility.
--
{-# DEPRECATED SingI "Use 'SListI' instead." #-}
{-# DEPRECATED sing "Use 'sList' instead." #-}
class SListI xs => SingI (xs :: [k]) where
  sing :: Sing xs

-- | Explicit singleton type.
--
-- Just provided for limited backward compatibility.
{-# DEPRECATED Sing "Use 'SList' instead." #-}
type Sing = SList

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

-- | The length of a type-level list.
--
-- @since 0.2
--
lengthSList :: forall (xs :: [k]) proxy. SListI xs => proxy xs -> Int
lengthSList _ = lengthShape (shape :: Shape xs)
  where
    lengthShape :: forall xs'. Shape xs' -> Int
    lengthShape ShapeNil      = 0
    lengthShape (ShapeCons s) = 1 + lengthShape s

-- | Old name for 'lengthSList'.
{-# DEPRECATED lengthSing "Use 'lengthSList' instead." #-}
lengthSing :: SListI xs => proxy xs -> Int
lengthSing = lengthSList
