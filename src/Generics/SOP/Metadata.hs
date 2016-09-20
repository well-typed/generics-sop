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
  ) where

import qualified Data.Vector as V
import GHC.Generics (Associativity(..))
import Unsafe.Coerce

import Generics.SOP.Constraint
import Generics.SOP.NP
import Generics.SOP.Sing

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
data DatatypeInfo :: [[*]] -> * where
  -- Standard algebraic datatype
  ADT     :: ModuleName -> DatatypeName -> NP ConstructorInfo xss -> DatatypeInfo xss
  -- Newtype
  Newtype :: ModuleName -> DatatypeName -> ConstructorInfo '[x]   -> DatatypeInfo '[ '[x] ]

moduleName :: DatatypeInfo xss -> ModuleName
moduleName (ADT name _ _) = name
moduleName (Newtype name _ _) = name

datatypeName :: DatatypeInfo xss -> DatatypeName
datatypeName (ADT _ name _ ) = name
datatypeName (Newtype _ name _) = name

constructorInfo :: DatatypeInfo xss -> NP ConstructorInfo xss
constructorInfo (ADT _ _ cs) = cs
constructorInfo (Newtype _ _ c) = NP (unsafeCoerce (V.singleton c))

{-
deriving instance All (Show `Compose` ConstructorInfo) xs => Show (DatatypeInfo xs)
deriving instance All (Eq   `Compose` ConstructorInfo) xs => Eq   (DatatypeInfo xs)
deriving instance (All (Eq `Compose` ConstructorInfo) xs, All (Ord `Compose` ConstructorInfo) xs) => Ord (DatatypeInfo xs)
-}

-- | Metadata for a single constructors.
--
-- This is indexed by the product structure of the constructor components.
--
data ConstructorInfo :: [*] -> * where
  -- Normal constructor
  Constructor :: SListI xs => ConstructorName -> ConstructorInfo xs
  -- Infix constructor
  Infix :: ConstructorName -> Associativity -> Fixity -> ConstructorInfo '[ x, y ]
  -- Record constructor
  Record :: SListI xs => ConstructorName -> NP FieldInfo xs -> ConstructorInfo xs

constructorName :: ConstructorInfo xs -> ConstructorName
constructorName (Constructor name) = name
constructorName (Infix name _ _)   = name
constructorName (Record name _)    = name

{-
deriving instance All (Show `Compose` FieldInfo) xs => Show (ConstructorInfo xs)
deriving instance All (Eq   `Compose` FieldInfo) xs => Eq   (ConstructorInfo xs)
deriving instance (All (Eq `Compose` FieldInfo) xs, All (Ord `Compose` FieldInfo) xs) => Ord (ConstructorInfo xs)
-}

-- | For records, this functor maps the component to its selector name.
data FieldInfo :: * -> * where
  FieldInfo :: FieldName -> FieldInfo a
  deriving (Show, Eq, Ord, Functor)

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

