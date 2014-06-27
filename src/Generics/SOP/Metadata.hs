{-# LANGUAGE StandaloneDeriving, UndecidableInstances #-}
-- | Metadata about what a datatype looks like
--
-- In @generics-sop@, the metadata is completely independent of the main
-- universe. Many generic functions will use this metadata, but other don't,
-- and yet others might need completely different metadata.
--
-- This module defines a datatype to represent standard metadata, i.e., names
-- of the datatype, its constructors, and possibly its record selectors.
-- Metadata descriptions are in general GADTs indexed by the code of the
-- datatype they're associated with, so matching on the metadata will reveal
-- information about the shape of the datatype.
--
module Generics.SOP.Metadata where

import Data.Proxy (Proxy(..))

import Generics.SOP.Constraint
import Generics.SOP.NP
import Generics.SOP.Sing
import Generics.SOP.Universe

-- | The name of a datatype.
type DatatypeName    = String

-- | The name of a module.
type ModuleName      = String

-- | The name of a data constructor.
type ConstructorName = String

-- | The name of a field / record selector.
type FieldName       = String

-- | Metadata for a datatype.
--
-- A value of type @DatatypeInfo c@ contains the information about a datatype
-- that is not contained in its 'Code' @c@. This information consists
-- primarily of the names of the datatype, its constructors, and possibly its
-- record selectors.
--
-- The constructor indicates whether the datatype has been declared using 'newtype'
-- or not.
--
data DatatypeInfo :: [[*]] -> * where
  -- Standard algebraic datatype
  ADT     :: ModuleName -> DatatypeName -> NP ConstructorInfo xss -> DatatypeInfo xss
  -- Newtype
  Newtype :: ModuleName -> DatatypeName -> ConstructorInfo '[x]   -> DatatypeInfo '[ '[x] ]

deriving instance All Show (Map ConstructorInfo xs) => Show (DatatypeInfo xs)
deriving instance All Eq   (Map ConstructorInfo xs) => Eq   (DatatypeInfo xs)
deriving instance (All Eq (Map ConstructorInfo xs), All Ord (Map ConstructorInfo xs)) => Ord (DatatypeInfo xs)

-- | Metadata for a single constructors.
--
-- This is indexed by the product structure of the constructor components.
--
data ConstructorInfo :: [*] -> * where
  -- Normal constructor
  Constructor :: SingI xs => ConstructorName -> ConstructorInfo xs
  -- Record constructor
  Record :: SingI xs => ConstructorName -> NP FieldInfo xs -> ConstructorInfo xs
-- TODO: Add support for InfixC

deriving instance All Show (Map FieldInfo xs) => Show (ConstructorInfo xs)
deriving instance All Eq   (Map FieldInfo xs) => Eq   (ConstructorInfo xs)
deriving instance (All Eq (Map FieldInfo xs), All Ord (Map FieldInfo xs)) => Ord (ConstructorInfo xs)

-- | For records, this functor maps the component to its selector name.
data FieldInfo :: * -> * where
  FieldInfo :: FieldName -> FieldInfo a
  deriving (Show, Eq, Ord, Functor)

-- | A class of datatypes that have associated metadata.
--
-- It is possible to use the sum-of-products approach to generic programming
-- without metadata. If you need metadata in a function, an additional
-- constraint on this class is in order.
--
-- You typically don't define instances of this class by hand, but use
-- 'deriveGeneric' and Template Haskell to generate that for you.
--
class HasDatatypeInfo a where
  datatypeInfo :: Proxy a -> DatatypeInfo (Code a)
