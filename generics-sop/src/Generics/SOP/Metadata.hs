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
module Generics.SOP.Metadata
  ( module Generics.SOP.Metadata
    -- * re-exports
  , Associativity(..)
  , DecidedStrictness(..)
  , SourceStrictness(..)
  , SourceUnpackedness(..)
  ) where

import Data.Kind (Type)
import GHC.Generics
  ( Associativity(..)
  , DecidedStrictness(..)
  , SourceStrictness(..)
  , SourceUnpackedness(..)
  )

import Generics.SOP.Constraint
import Generics.SOP.NP

-- | Metadata for a datatype.
--
-- A value of type @'DatatypeInfo' c@ contains the information about a datatype
-- that is not contained in @'Code' c@. This information consists
-- primarily of the names of the datatype, its constructors, and possibly its
-- record selectors.
--
-- The constructor indicates whether the datatype has been declared using @newtype@
-- or not.
--
data DatatypeInfo :: [[Type]] -> Type where
  -- Standard algebraic datatype
  ADT     ::
       ModuleName
    -> DatatypeName
    -> NP ConstructorInfo xss
    -> POP StrictnessInfo xss
    -> DatatypeInfo xss
  -- Newtype
  Newtype ::
       ModuleName
    -> DatatypeName
    -> ConstructorInfo '[x]
    -> DatatypeInfo '[ '[x] ]

-- | The module name where a datatype is defined.
--
-- @since 0.2.3.0
--
moduleName :: DatatypeInfo xss -> ModuleName
moduleName (ADT name _ _ _) = name
moduleName (Newtype name _ _) = name

-- | The name of a datatype (or newtype).
--
-- @since 0.2.3.0
--
datatypeName :: DatatypeInfo xss -> DatatypeName
datatypeName (ADT _ name _ _) = name
datatypeName (Newtype _ name _) = name

-- | The constructor info for a datatype (or newtype).
--
-- @since 0.2.3.0
--
constructorInfo :: DatatypeInfo xss -> NP ConstructorInfo xss
constructorInfo (ADT _ _ cs _) = cs
constructorInfo (Newtype _ _ c) = c :* Nil

deriving instance
  ( All (Show `Compose` ConstructorInfo) xs
  , All (Show `Compose` NP StrictnessInfo) xs
  ) => Show (DatatypeInfo xs)
deriving instance
  ( All (Eq `Compose` ConstructorInfo) xs
  , All (Eq `Compose` NP StrictnessInfo) xs
  ) => Eq (DatatypeInfo xs)
deriving instance
  ( All (Eq `Compose` ConstructorInfo) xs
  , All (Ord `Compose` ConstructorInfo) xs
  , All (Eq `Compose` NP StrictnessInfo) xs
  , All (Ord `Compose` NP StrictnessInfo) xs
  ) => Ord (DatatypeInfo xs)

-- | Metadata for a single constructor.
--
-- This is indexed by the product structure of the constructor components.
--
data ConstructorInfo :: [Type] -> Type where
  -- Normal constructor
  Constructor :: SListI xs => ConstructorName -> ConstructorInfo xs
  -- Infix constructor
  Infix :: ConstructorName -> Associativity -> Fixity -> ConstructorInfo '[ x, y ]
  -- Record constructor
  Record :: SListI xs => ConstructorName -> NP FieldInfo xs -> ConstructorInfo xs

-- | The name of a constructor.
--
-- @since 0.2.3.0
--
constructorName :: ConstructorInfo xs -> ConstructorName
constructorName (Constructor name) = name
constructorName (Infix name _ _)   = name
constructorName (Record name _)    = name

deriving instance All (Show `Compose` FieldInfo) xs => Show (ConstructorInfo xs)
deriving instance All (Eq   `Compose` FieldInfo) xs => Eq   (ConstructorInfo xs)
deriving instance (All (Eq `Compose` FieldInfo) xs, All (Ord `Compose` FieldInfo) xs) => Ord (ConstructorInfo xs)

-- | Metadata for strictness information of a field.
--
-- Indexed by the type of the field.
--
-- @since 0.4.0.0
--
data StrictnessInfo :: Type -> Type where
  StrictnessInfo ::
       SourceUnpackedness
    -> SourceStrictness
    -> DecidedStrictness
    -> StrictnessInfo a
  deriving (Show, Eq, Ord, Functor)

-- | For records, this functor maps the component to its selector name.
data FieldInfo :: Type -> Type where
  FieldInfo :: FieldName -> FieldInfo a
  deriving (Show, Eq, Ord, Functor)

-- | The name of a field.
--
-- @since 0.2.3.0
--
fieldName :: FieldInfo a -> FieldName
fieldName (FieldInfo n) = n

-- | The name of a datatype.
type DatatypeName    = String

-- | The name of a module.
type ModuleName      = String

-- | The name of a data constructor.
type ConstructorName = String

-- | The name of a field / record selector.
type FieldName       = String

-- | The fixity of an infix constructor.
type Fixity          = Int

