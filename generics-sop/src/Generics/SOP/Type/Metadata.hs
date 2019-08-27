{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
-- | Type-level metadata
--
-- This module provides datatypes (to be used promoted) that can represent the
-- metadata of Haskell datatypes on the type level.
--
-- We do not reuse the term-level metadata types, because these are GADTs that
-- incorporate additional invariants. We could (at least in GHC 8) impose the
-- same invariants on the type level as well, but some tests have revealed that
-- the resulting type are rather inconvenient to work with.
--
-- So we use simple datatypes to represent the type-level metadata, even if
-- this means that some invariants are not explicitly captured.
--
-- We establish a relation between the term- and type-level versions of the
-- metadata by automatically computing the term-level version from the type-level
-- version.
--
-- As we now have two versions of metadata (term-level and type-level)
-- with very similar, yet slightly different datatype definitions, the names
-- between the modules clash, and this module is recommended to be imported
-- qualified when needed.
--
-- The interface exported by this module is still somewhat experimental.
--
-- @since 0.3.0.0
--
module Generics.SOP.Type.Metadata
  ( module Generics.SOP.Type.Metadata
    -- * re-exports
  , Associativity(..)
  ) where

#if __GLASGOW_HASKELL__ <802
import Data.Kind (Type)
#endif
import Data.Proxy (Proxy (..))
import GHC.Generics
  ( Associativity(..)
  , DecidedStrictness(..)
  , SourceStrictness(..)
  , SourceUnpackedness(..)
  )
import GHC.Types
import GHC.TypeLits

import qualified Generics.SOP.Metadata as M
import Generics.SOP.NP
import Generics.SOP.Sing

-- Regarding the CPP in the datatype definitions below:
--
-- We cannot promote type synonyms in GHC 7, so we
-- use equivalent yet less descriptive definitions
-- for the older GHCs.

-- | Metadata for a datatype (to be used promoted).
--
-- A type of kind @'DatatypeInfo'@ contains meta-information about a datatype
-- that is not contained in its code. This information consists
-- primarily of the names of the datatype, its constructors, and possibly its
-- record selectors.
--
-- The constructor indicates whether the datatype has been declared using @newtype@
-- or not.
--
-- @since 0.3.0.0
--
data DatatypeInfo =
    ADT ModuleName DatatypeName [ConstructorInfo] [[StrictnessInfo]]
    -- ^ Standard algebraic datatype
  | Newtype ModuleName DatatypeName ConstructorInfo
    -- ^ Newtype

-- | Metadata for a single constructors (to be used promoted).
--
-- @since 0.3.0.0
--
data ConstructorInfo =
    Constructor ConstructorName
    -- ^ Normal constructor
  | Infix ConstructorName Associativity Fixity
    -- ^ Infix constructor
  | Record ConstructorName [FieldInfo]
    -- ^ Record constructor

-- | Strictness information for a single field (to be used promoted).
--
-- @since 0.4.0.0
--
data StrictnessInfo =
    StrictnessInfo SourceUnpackedness SourceStrictness DecidedStrictness

-- | Metadata for a single record field (to be used promoted).
--
-- @since 0.3.0.0
--
data FieldInfo =
    FieldInfo FieldName

-- | The name of a datatype.
type DatatypeName    = Symbol

-- | The name of a module.
type ModuleName      = Symbol

-- | The name of a data constructor.
type ConstructorName = Symbol

-- | The name of a field / record selector.
type FieldName       = Symbol

-- | The fixity of an infix constructor.
type Fixity          = Nat

-- Demotion
--
-- The following classes are concerned with computing the
-- term-level metadata from the type-level metadata.

-- | Class for computing term-level datatype information from
-- type-level datatype information.
--
-- @since 0.3.0.0
--
class DemoteDatatypeInfo (x :: DatatypeInfo) (xss :: [[Type]]) where
  -- | Given a proxy of some type-level datatype information,
  -- return the corresponding term-level information.
  --
  -- @since 0.3.0.0
  --
  demoteDatatypeInfo :: proxy x -> M.DatatypeInfo xss

instance
     ( KnownSymbol m
     , KnownSymbol d
     , DemoteConstructorInfos cs xss
     , DemoteStrictnessInfoss sss xss
     )
  => DemoteDatatypeInfo ('ADT m d cs sss) xss where
  demoteDatatypeInfo _ =
    M.ADT
      (symbolVal (Proxy :: Proxy m))
      (symbolVal (Proxy :: Proxy d))
      (demoteConstructorInfos (Proxy :: Proxy cs))
      (POP (demoteStrictnessInfoss (Proxy :: Proxy sss)))

instance
     (KnownSymbol m, KnownSymbol d, DemoteConstructorInfo c '[ x ])
  => DemoteDatatypeInfo ('Newtype m d c) '[ '[ x ] ] where
  demoteDatatypeInfo _ =
    M.Newtype
      (symbolVal (Proxy :: Proxy m))
      (symbolVal (Proxy :: Proxy d))
      (demoteConstructorInfo (Proxy :: Proxy c))

-- | Class for computing term-level constructor information from
-- type-level constructor information.
--
-- @since 0.3.0.0
--
class DemoteConstructorInfos (cs :: [ConstructorInfo]) (xss :: [[Type]]) where
  -- | Given a proxy of some type-level constructor information,
  -- return the corresponding term-level information as a product.
  --
  -- @since 0.3.0.0
  --
  demoteConstructorInfos :: proxy cs -> NP M.ConstructorInfo xss

instance DemoteConstructorInfos '[] '[] where
  demoteConstructorInfos _ = Nil

instance
     (DemoteConstructorInfo c xs, DemoteConstructorInfos cs xss)
  => DemoteConstructorInfos (c ': cs) (xs ': xss) where
  demoteConstructorInfos _ =
    demoteConstructorInfo (Proxy :: Proxy c) :* demoteConstructorInfos (Proxy :: Proxy cs)

-- | Class for computing term-level constructor information from
-- type-level constructor information.
--
-- @since 0.3.0.0
--
class DemoteConstructorInfo (x :: ConstructorInfo) (xs :: [Type]) where
  -- | Given a proxy of some type-level constructor information,
  -- return the corresponding term-level information.
  --
  -- @since 0.3.0.0
  --
  demoteConstructorInfo :: proxy x -> M.ConstructorInfo xs

instance (KnownSymbol s, SListI xs) => DemoteConstructorInfo ('Constructor s) xs where
  demoteConstructorInfo _ = M.Constructor (symbolVal (Proxy :: Proxy s))

instance
     (KnownSymbol s, DemoteAssociativity a, KnownNat f)
  => DemoteConstructorInfo ('Infix s a f) [y, z] where
  demoteConstructorInfo _ =
    M.Infix
      (symbolVal (Proxy :: Proxy s))
      (demoteAssociativity (Proxy :: Proxy a))
      (fromInteger (natVal (Proxy :: Proxy f)))

instance (KnownSymbol s, DemoteFieldInfos fs xs) => DemoteConstructorInfo ('Record s fs) xs where
  demoteConstructorInfo _ =
    M.Record (symbolVal (Proxy :: Proxy s)) (demoteFieldInfos (Proxy :: Proxy fs))


class DemoteStrictnessInfoss (sss :: [[StrictnessInfo]]) (xss :: [[Type]]) where
  demoteStrictnessInfoss :: proxy sss -> NP (NP M.StrictnessInfo) xss

instance DemoteStrictnessInfoss '[] '[] where
  demoteStrictnessInfoss _ = Nil

instance
     (DemoteStrictnessInfos ss xs, DemoteStrictnessInfoss sss xss)
  => DemoteStrictnessInfoss (ss ': sss) (xs ': xss) where
  demoteStrictnessInfoss _ =
       demoteStrictnessInfos  (Proxy :: Proxy ss )
    :* demoteStrictnessInfoss (Proxy :: Proxy sss)

class DemoteStrictnessInfos (ss :: [StrictnessInfo]) (xs :: [Type]) where
  demoteStrictnessInfos :: proxy ss -> NP M.StrictnessInfo xs

instance DemoteStrictnessInfos '[] '[] where
  demoteStrictnessInfos _ = Nil

instance
     (DemoteStrictnessInfo s x, DemoteStrictnessInfos ss xs)
  => DemoteStrictnessInfos (s ': ss) (x ': xs) where
  demoteStrictnessInfos _ =
       demoteStrictnessInfo  (Proxy :: Proxy s )
    :* demoteStrictnessInfos (Proxy :: Proxy ss)

class DemoteStrictnessInfo (s :: StrictnessInfo) (x :: Type) where
  demoteStrictnessInfo :: proxy s -> M.StrictnessInfo x

instance
     ( DemoteSourceUnpackedness su
     , DemoteSourceStrictness   ss
     , DemoteDecidedStrictness  ds
     )
  => DemoteStrictnessInfo ('StrictnessInfo su ss ds) x where
  demoteStrictnessInfo _ =
    M.StrictnessInfo
      (demoteSourceUnpackedness (Proxy :: Proxy su))
      (demoteSourceStrictness   (Proxy :: Proxy ss))
      (demoteDecidedStrictness  (Proxy :: Proxy ds))

-- | Class for computing term-level field information from
-- type-level field information.
--
-- @since 0.3.0.0
--
class SListI xs => DemoteFieldInfos (fs :: [FieldInfo]) (xs :: [Type]) where
  -- | Given a proxy of some type-level field information,
  -- return the corresponding term-level information as a product.
  --
  -- @since 0.3.0.0
  --
  demoteFieldInfos :: proxy fs -> NP M.FieldInfo xs

instance DemoteFieldInfos '[] '[] where
  demoteFieldInfos _ = Nil

instance
     (DemoteFieldInfo f x, DemoteFieldInfos fs xs)
  => DemoteFieldInfos (f ': fs) (x ': xs) where
  demoteFieldInfos _ = demoteFieldInfo (Proxy :: Proxy f) :* demoteFieldInfos (Proxy :: Proxy fs)

-- | Class for computing term-level field information from
-- type-level field information.
--
-- @since 0.3.0.0
--
class DemoteFieldInfo (x :: FieldInfo) (a :: Type) where
  -- | Given a proxy of some type-level field information,
  -- return the corresponding term-level information.
  --
  -- @since 0.3.0.0
  --
  demoteFieldInfo :: proxy x -> M.FieldInfo a

instance KnownSymbol s => DemoteFieldInfo ('FieldInfo s) a where
  demoteFieldInfo _ = M.FieldInfo (symbolVal (Proxy :: Proxy s))

-- | Class for computing term-level associativity information
-- from type-level associativity information.
--
-- @since 0.3.0.0
--
class DemoteAssociativity (a :: Associativity) where
  -- | Given a proxy of some type-level associativity information,
  -- return the corresponding term-level information.
  --
  -- @since 0.3.0.0
  --
  demoteAssociativity :: proxy a -> M.Associativity

instance DemoteAssociativity 'LeftAssociative where
  demoteAssociativity _ = M.LeftAssociative

instance DemoteAssociativity 'RightAssociative where
  demoteAssociativity _ = M.RightAssociative

instance DemoteAssociativity 'NotAssociative where
  demoteAssociativity _ = M.NotAssociative

-- | Class for computing term-level source unpackedness information
-- from type-level source unpackedness information.
--
-- @since 0.4.0.0
--
class DemoteSourceUnpackedness (a :: SourceUnpackedness) where
  -- | Given a proxy of some type-level source unpackedness information,
  -- return the corresponding term-level information.
  --
  -- @since 0.4.0.0
  --
  demoteSourceUnpackedness :: proxy a -> M.SourceUnpackedness

instance DemoteSourceUnpackedness 'NoSourceUnpackedness where
  demoteSourceUnpackedness _ = M.NoSourceUnpackedness

instance DemoteSourceUnpackedness 'SourceNoUnpack where
  demoteSourceUnpackedness _ = M.SourceNoUnpack

instance DemoteSourceUnpackedness 'SourceUnpack where
  demoteSourceUnpackedness _ = M.SourceUnpack

-- | Class for computing term-level source strictness information
-- from type-level source strictness information.
--
-- @since 0.4.0.0
--
class DemoteSourceStrictness (a :: SourceStrictness) where
  -- | Given a proxy of some type-level source strictness information,
  -- return the corresponding term-level information.
  --
  -- @since 0.4.0.0
  --
  demoteSourceStrictness :: proxy a -> M.SourceStrictness

instance DemoteSourceStrictness 'NoSourceStrictness where
  demoteSourceStrictness _ = M.NoSourceStrictness

instance DemoteSourceStrictness 'SourceLazy where
  demoteSourceStrictness _ = M.SourceLazy

instance DemoteSourceStrictness 'SourceStrict where
  demoteSourceStrictness _ = M.SourceStrict

-- | Class for computing term-level decided strictness information
-- from type-level decided strictness information.
--
-- @since 0.4.0.0
--
class DemoteDecidedStrictness (a :: DecidedStrictness) where
  -- | Given a proxy of some type-level source strictness information,
  -- return the corresponding term-level information.
  --
  -- @since 0.4.0.0
  --
  demoteDecidedStrictness :: proxy a -> M.DecidedStrictness

instance DemoteDecidedStrictness 'DecidedLazy where
  demoteDecidedStrictness _ = M.DecidedLazy

instance DemoteDecidedStrictness 'DecidedStrict where
  demoteDecidedStrictness _ = M.DecidedStrict

instance DemoteDecidedStrictness 'DecidedUnpack where
  demoteDecidedStrictness _ = M.DecidedUnpack

