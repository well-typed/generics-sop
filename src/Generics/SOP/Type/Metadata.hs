{-# LANGUAGE TypeInType, UndecidableInstances #-}
module Generics.SOP.Type.Metadata where

import Data.Proxy
import GHC.Generics (Associativity(..))
import GHC.Types
import GHC.TypeLits

import qualified Generics.SOP.Metadata as M
import Generics.SOP.NP
import Generics.SOP.Sing
import Generics.SOP.Universe

data DatatypeInfo :: [[Type]] -> Type where
  -- Standard algebraic datatype
  ADT     :: ModuleName -> DatatypeName -> NP ConstructorInfo xss -> DatatypeInfo xss
  -- Newtype
  Newtype :: ModuleName -> DatatypeName -> ConstructorInfo '[x]   -> DatatypeInfo '[ '[x] ]

data ConstructorInfo :: [Type] -> Type where
  -- Normal constructor
  Constructor :: ConstructorName -> ConstructorInfo xs
  -- Infix constructor
  Infix :: ConstructorName -> Associativity -> Fixity -> ConstructorInfo '[ x, y ]
  -- Record constructor
  Record :: ConstructorName -> NP FieldInfo xs -> ConstructorInfo xs

data FieldInfo :: Type -> Type where
  FieldInfo :: FieldName -> FieldInfo a

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

type family Demoted (k :: Type) :: Type
type instance Demoted (FieldInfo x)        = M.FieldInfo x
type instance Demoted (ConstructorInfo xs) = M.ConstructorInfo xs
type instance Demoted (DatatypeInfo xss)   = M.DatatypeInfo xss
type instance Demoted (NP f xs)            = NP (WrapDemoted f) xs
type instance Demoted Symbol               = String
type instance Demoted Nat                  = Int
type instance Demoted Associativity        = Associativity

newtype WrapDemoted f a = WrappedDemoted { unWrapDemoted :: Demoted (f a) }

class Demote (a :: k) where
  demote :: proxy a -> Demoted k

instance KnownSymbol x => Demote x where
  demote p = symbolVal p

instance KnownNat x => Demote x where
  demote p = fromInteger (natVal p)

instance Demote x => Demote ('FieldInfo x) where
  demote _ = M.FieldInfo (demote (Proxy :: Proxy x))

instance (Demote x, SListI xs) => Demote ('Constructor x :: ConstructorInfo xs) where
  demote _ = M.Constructor (demote (Proxy :: Proxy x))

instance (Demote x, Demote a, Demote f) => Demote ('Infix x a f) where
  demote _ =
    M.Infix
      (demote (Proxy :: Proxy x))
      (demote (Proxy :: Proxy a))
      (demote (Proxy :: Proxy f))

instance (Demote x, Demote fis, SListI xs) => Demote ('Record x fis :: ConstructorInfo xs) where
  demote _ =
    M.Record
      (demote (Proxy :: Proxy x))
      (map_NP unWrapDemoted (demote (Proxy :: Proxy fis))) -- TODO: coerce?

instance Demote 'Nil where
  demote _ = Nil

instance (Demote x, Demote xs) => Demote (x ':* xs) where
  demote _ = WrappedDemoted (demote (Proxy :: Proxy x)) :* demote (Proxy :: Proxy xs)

instance Demote 'LeftAssociative where
  demote _ = LeftAssociative

instance Demote 'RightAssociative where
  demote _ = RightAssociative

instance Demote 'NotAssociative where
  demote _ = NotAssociative

instance (Demote m, Demote d, Demote cis, SListI xss) => Demote ('ADT m d cis :: DatatypeInfo xss) where
  demote _ =
    M.ADT
      (demote (Proxy :: Proxy m))
      (demote (Proxy :: Proxy d))
      (map_NP unWrapDemoted (demote (Proxy :: Proxy cis)))

instance (Demote m, Demote d, Demote ci) => Demote ('Newtype m d ci) where
  demote _ =
    M.Newtype
      (demote (Proxy :: Proxy m))
      (demote (Proxy :: Proxy d))
      (demote (Proxy :: Proxy ci))

type family DatatypeInfoOf (a :: Type) :: DatatypeInfo (Code a)

instance {-# OVERLAPPABLE #-} (Demote (DatatypeInfoOf a)) => HasDatatypeInfo a where
  datatypeInfo _ = demote (Proxy :: Proxy (DatatypeInfoOf a))
