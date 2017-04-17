{-# LANGUAGE PolyKinds, UndecidableInstances #-}
module Generics.SOP.Type.Metadata
  ( module Generics.SOP.Type.Metadata
    -- * re-exports
  , Associativity(..)
  ) where

import Data.Proxy
import GHC.Generics (Associativity(..))
#if __GLASGOW_HASKELL__ >= 800
import GHC.Types
#endif
import GHC.TypeLits

import qualified Generics.SOP.Metadata as M
import Generics.SOP.NP
import Generics.SOP.Sing

data DatatypeInfo =
#if __GLASGOW_HASKELL__ >= 800
    ADT ModuleName DatatypeName [ConstructorInfo]
    -- ^ Standard algebraic datatype
  | Newtype ModuleName DatatypeName ConstructorInfo
    -- ^ Newtype
#else
    ADT Symbol Symbol [ConstructorInfo]
    -- ^ Standard algebraic datatype
  | Newtype Symbol Symbol ConstructorInfo
    -- ^ Newtype
#endif

data ConstructorInfo =
#if __GLASGOW_HASKELL__ >= 800
    Constructor ConstructorName
    -- ^ Normal constructor
  | Infix ConstructorName Associativity Fixity
    -- ^ Infix constructor
  | Record ConstructorName [FieldInfo]
    -- ^ Record constructor
#else
    Constructor Symbol
    -- ^ Normal constructor
  | Infix Symbol Associativity Nat
    -- ^ Infix constructor
  | Record Symbol [FieldInfo]
    -- ^ Record constructor
#endif

data FieldInfo =
#if __GLASGOW_HASKELL__ >= 800
    FieldInfo FieldName
#else
    FieldInfo Symbol
#endif

#if __GLASGOW_HASKELL__ >= 800
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
#endif

class DemoteDatatypeInfo (x :: DatatypeInfo) (xss :: [[*]]) where
  demoteDatatypeInfo :: proxy x -> M.DatatypeInfo xss

instance
     (KnownSymbol m, KnownSymbol d, DemoteConstructorInfos cs xss)
  => DemoteDatatypeInfo ('ADT m d cs) xss where
  demoteDatatypeInfo _ =
    M.ADT
      (symbolVal (Proxy :: Proxy m))
      (symbolVal (Proxy :: Proxy d))
      (demoteConstructorInfos (Proxy :: Proxy cs))

instance
     (KnownSymbol m, KnownSymbol d, DemoteConstructorInfo c '[ x ])
  => DemoteDatatypeInfo ('Newtype m d c) '[ '[ x ] ] where
  demoteDatatypeInfo _ =
    M.Newtype
      (symbolVal (Proxy :: Proxy m))
      (symbolVal (Proxy :: Proxy d))
      (demoteConstructorInfo (Proxy :: Proxy c))

class DemoteConstructorInfos (cs :: [ConstructorInfo]) (xss :: [[*]]) where
  demoteConstructorInfos :: proxy cs -> NP M.ConstructorInfo xss

instance DemoteConstructorInfos '[] '[] where
  demoteConstructorInfos _ = Nil

instance
     (DemoteConstructorInfo c xs, DemoteConstructorInfos cs xss)
  => DemoteConstructorInfos (c ': cs) (xs ': xss) where
  demoteConstructorInfos _ =
    demoteConstructorInfo (Proxy :: Proxy c) :* demoteConstructorInfos (Proxy :: Proxy cs)

class DemoteConstructorInfo (x :: ConstructorInfo) (xs :: [*]) where
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

class SListI xs => DemoteFieldInfos (fs :: [FieldInfo]) (xs :: [*]) where
  demoteFieldInfos :: proxy fs -> NP M.FieldInfo xs

instance DemoteFieldInfos '[] '[] where
  demoteFieldInfos _ = Nil

instance
     (DemoteFieldInfo f x, DemoteFieldInfos fs xs)
  => DemoteFieldInfos (f ': fs) (x ': xs) where
  demoteFieldInfos _ = demoteFieldInfo (Proxy :: Proxy f) :* demoteFieldInfos (Proxy :: Proxy fs)

class DemoteFieldInfo (x :: FieldInfo) (a :: *) where
  demoteFieldInfo :: proxy x -> M.FieldInfo a

class DemoteAssociativity (a :: Associativity) where
  demoteAssociativity :: proxy a -> M.Associativity

instance DemoteAssociativity 'LeftAssociative where
  demoteAssociativity _ = M.LeftAssociative

instance DemoteAssociativity 'RightAssociative where
  demoteAssociativity _ = M.RightAssociative

instance DemoteAssociativity 'NotAssociative where
  demoteAssociativity _ = M.NotAssociative

instance KnownSymbol s => DemoteFieldInfo ('FieldInfo s) a where
  demoteFieldInfo _ = M.FieldInfo (symbolVal (Proxy :: Proxy s))

