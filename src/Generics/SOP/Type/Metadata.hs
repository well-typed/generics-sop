{-# LANGUAGE TypeInType, UndecidableInstances #-}
module Generics.SOP.Type.Metadata where

import Data.Proxy
import GHC.Generics (Associativity(..))
import GHC.Types
import GHC.TypeLits

import qualified Generics.SOP.Metadata as M
import Generics.SOP.NP
import Generics.SOP.Sing
-- import Generics.SOP.Universe

data DatatypeInfo :: Type where
  -- Standard algebraic datatype
  ADT     :: ModuleName -> DatatypeName -> [ConstructorInfo] -> DatatypeInfo
  -- Newtype
  Newtype :: ModuleName -> DatatypeName -> ConstructorInfo   -> DatatypeInfo

data ConstructorInfo :: Type where
  -- Normal constructor
  Constructor :: ConstructorName -> ConstructorInfo
  -- Infix constructor
  Infix :: ConstructorName -> Associativity -> Fixity -> ConstructorInfo
  -- Record constructor
  Record :: ConstructorName -> [FieldInfo] -> ConstructorInfo

data FieldInfo :: Type where
  FieldInfo :: FieldName -> FieldInfo

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

type family Demoted (k :: Type) (c :: r) :: Type
type instance Demoted DatatypeInfo xss     = M.DatatypeInfo xss
type instance Demoted ConstructorInfo xs   = M.ConstructorInfo xs
type instance Demoted FieldInfo x          = M.FieldInfo x
type instance Demoted [f] xs               = NP (WrapDemoted f) xs
type instance Demoted Symbol _             = String
type instance Demoted Nat _                = Int
type instance Demoted Associativity _      = Associativity

newtype WrapDemoted f a = WrappedDemoted { unWrapDemoted :: Demoted f a }

class Demote (a :: k) (c :: r) where
  demote :: proxy1 a -> proxy2 c -> Demoted k c

instance KnownSymbol x => Demote x c where
  demote p _ = symbolVal p

instance KnownNat x => Demote x c where
  demote p _ = fromInteger (natVal p)

instance Demote x '() => Demote ('FieldInfo x :: FieldInfo) (a :: Type) where
  demote _ _ = M.FieldInfo (demote (Proxy :: Proxy x) (Proxy :: Proxy '())) :: M.FieldInfo a

instance (Demote x '(), SListI xs) => Demote ('Constructor x :: ConstructorInfo) (xs :: [Type]) where
  demote _ _ = M.Constructor (demote (Proxy :: Proxy x) (Proxy :: Proxy '()))

instance (Demote x '(), Demote a '(), Demote f '()) => Demote ('Infix x a f :: ConstructorInfo) ('[y, z] :: [Type]) where
  demote _ _ =
    M.Infix
      (demote (Proxy :: Proxy x) (Proxy :: Proxy '()))
      (demote (Proxy :: Proxy a) (Proxy :: Proxy '()))
      (demote (Proxy :: Proxy f) (Proxy :: Proxy '()))

instance (Demote x '(), Demote fis xs, SListI xs) => Demote ('Record x fis :: ConstructorInfo) (xs :: [Type]) where
  demote _ _ =
    M.Record
      (demote (Proxy :: Proxy x) (Proxy :: Proxy '()))
      (map_NP unWrapDemoted (demote (Proxy :: Proxy fis) (Proxy :: Proxy xs))) -- TODO: coerce?

instance Demote '[] '[] where
  demote _ _ = Nil

instance (Demote x i, Demote xs is) => Demote (x ': xs) (i ': is) where
  demote _ _ =
       WrappedDemoted (demote (Proxy :: Proxy x) (Proxy :: Proxy i))
    :* demote (Proxy :: Proxy xs) (Proxy :: Proxy is)

instance Demote 'LeftAssociative c where
  demote _ _ = LeftAssociative

instance Demote 'RightAssociative c where
  demote _ _ = RightAssociative

instance Demote 'NotAssociative c where
  demote _ _ = NotAssociative

instance (Demote m '(), Demote d '(), Demote cis xss, SListI xss) => Demote ('ADT m d cis :: DatatypeInfo) (xss :: [[Type]]) where
  demote _ _ =
    M.ADT
      (demote (Proxy :: Proxy m) (Proxy :: Proxy '()))
      (demote (Proxy :: Proxy d) (Proxy :: Proxy '()))
      (map_NP unWrapDemoted (demote (Proxy :: Proxy cis) (Proxy :: Proxy xss)))

instance (Demote m '(), Demote d '(), Demote ci '[ x ], xss ~ '[ '[x] ]) => Demote ('Newtype m d ci :: DatatypeInfo) (xss :: [[Type]]) where
  demote _ _ =
    M.Newtype
      (demote (Proxy :: Proxy m) (Proxy :: Proxy '()))
      (demote (Proxy :: Proxy d) (Proxy :: Proxy '()))
      (demote (Proxy :: Proxy ci) (Proxy :: Proxy '[ x ]))

-- type family DatatypeInfoOf (a :: Type) :: DatatypeInfo

-- instance {-# OVERLAPPABLE #-} (Demote (DatatypeInfoOf a) (Code a)) => HasDatatypeInfo a where
--   datatypeInfo _ = demote (Proxy :: Proxy (DatatypeInfoOf a)) (Proxy :: Proxy (Code a))
