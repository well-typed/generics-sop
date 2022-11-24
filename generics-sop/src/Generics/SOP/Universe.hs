{-# LANGUAGE InstanceSigs            #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | Codes and interpretations
module Generics.SOP.Universe where

import Data.Kind (Type)
import Data.Coerce (Coercible, coerce)
import Data.Proxy
import qualified GHC.Generics as GHC

import Generics.SOP.BasicFunctors
import Generics.SOP.Constraint
import Generics.SOP.GGP
import Generics.SOP.Metadata
import Generics.SOP.NP
import Generics.SOP.NS
import qualified GHC.Generics.Generically   as GHC
import qualified Generics.SOP.Type.Metadata as T

-- | The (generic) representation of a datatype.
--
-- A datatype is isomorphic to the sum-of-products of its code.
-- The isomorphism is witnessed by 'from' and 'to' from the
-- 'Generic' class.
--
type Rep a = SOP I (Code a)

-- | The class of representable datatypes.
--
-- The SOP approach to generic programming is based on viewing
-- datatypes as a representation ('Rep') built from the sum of
-- products of its components. The components of a datatype
-- are specified using the 'Code' type family.
--
-- The isomorphism between the original Haskell datatype and its
-- representation is witnessed by the methods of this class,
-- 'from' and 'to'. So for instances of this class, the following
-- laws should (in general) hold:
--
-- @
-- 'to' '.' 'from' === 'id' :: a -> a
-- 'from' '.' 'to' === 'id' :: 'Rep' a -> 'Rep' a
-- @
--
-- Implementations that are based on 'Generic' are given to the
-- 'Generically' datatype.
--
-- You typically don't define instances of this class by hand, but
-- rather derive the class instance automatically.
--
-- /Option 1:/ Derive via the built-in GHC-generics. For this, you
-- need to use the @DeriveGeneric@ extension to first derive an
-- instance of the 'GHC.Generics.Generic' class from module "GHC.Generics".
-- With this you can derive it via 'GHC.Generics.Generically' or give an
-- empty instance for 'Generic' (deriving @anyclass@) and
-- the default definitions will just work. The pattern looks as
-- follows:
--
-- @
-- {-# Language DerivingVia        #-}
-- {-# Language DerivingStrategies #-}
--
-- import qualified "GHC.Generics" as GHC
-- import "Generics.SOP"
--
-- ...
--
-- data T = ...
--   deriving
--   stock GHC.'GHC.Generics.Generic'
--
--   deriving
--     ( Generic
--     , HasDatatypeInfo   --^ if you want metadata
--     )
--   via GHC.Generically T
-- @
--
-- /Option 2:/ Derive via Template Haskell. For this, you need to
-- enable the @TemplateHaskell@ extension. You can then use
-- 'Generics.SOP.TH.deriveGeneric' from module "Generics.SOP.TH"
-- to have the instance generated for you. The pattern looks as
-- follows:
--
-- @
-- import "Generics.SOP"
-- import "Generics.SOP.TH"
--
-- ...
--
-- data T = ...
--
-- 'Generics.SOP.TH.deriveGeneric' \'\'T -- derives 'HasDatatypeInfo' as well
-- @
--
-- /Tradeoffs:/ Whether to use Option 1 or 2 is mainly a matter
-- of personal taste. The version based on Template Haskell probably
-- has less run-time overhead.
--
-- /Non-standard instances:/
-- It is possible to give 'Generic' instances manually that deviate
-- from the standard scheme, as long as at least
--
-- @
-- 'to' '.' 'from' === 'id' :: a -> a
-- @
--
-- still holds.
--
class (All SListI (Code a)) => Generic (a :: Type) where
  -- | The code of a datatype.
  --
  -- This is a list of lists of its components. The outer list contains
  -- one element per constructor. The inner list contains one element
  -- per constructor argument (field).
  --
  -- /Example:/ The datatype
  --
  -- > data Tree = Leaf Int | Node Tree Tree
  --
  -- is supposed to have the following code:
  --
  -- > type instance Code (Tree a) =
  -- >   '[ '[ Int ]
  -- >    , '[ Tree, Tree ]
  -- >    ]
  --
  type Code a :: [[Type]]
  type Code a = GCode a

  -- | Converts from a value to its structural representation.
  from         :: a -> Rep a
  default from :: (GFrom a, GHC.Generic a, Rep a ~ SOP I (GCode a))
               => a -> Rep a
  from = gfrom

  -- | Converts from a structural representation back to the
  -- original value.
  to         :: Rep a -> a
  default to :: (GTo a, GHC.Generic a, Rep a ~ SOP I (GCode a))
             => Rep a -> a
  to = gto

-- | A generic ('GHC.Generics') implementation of SOP generic
-- behaviour.
--
-- @
-- {-# Language DerivingStrategies #-}
-- {-# Language DerivingVia        #-}
--
-- import qualified GHC.Generics as GHC
-- import qualified Generics.SOP as SOP
--
-- data T = ...
--   deriving
--   stock GHC.Generic
--
--   deriving (SOP.Generic, SOP.HasDatatypeInfo)
--   via GHC.Generically T
-- @
instance (GHC.Generic a, GFrom a, GTo a, All SListI (GCode a)) => Generic (GHC.Generically a) where
  type Code (GHC.Generically a) = GCode a

  from :: GHC.Generically a -> Rep (GHC.Generically a)
  from (GHC.Generically a) = gfrom a

  to :: Rep (GHC.Generically a) -> GHC.Generically a
  to rep = GHC.Generically (gto rep)

-- | An implementation of 'GHC.Generics.Generically' for
-- 'Generics.SOP.Generics'. Should be imported qualified when used
-- with 'GHC.Generics':
--
-- @
-- import Generics.SOP hiding (Generic, Generically)
-- import GHC.Generics hiding (Generic, Generically)
-- import qualified Generics.SOP as SOP
-- import qualified GHC.Generics as GHC
-- @
--
-- Type classes that have SOP-generic behaviour should be given an
-- instance of this newtype.
--
-- @
-- instance (SOP.Generic a, All2 Eq (Code a)) => Eq (SOP.Generically a) where
--   (==) :: SOP.Generically a -> SOP.Generically a -> Bool
--   SOP.Generically a == SOP.Generically b = geq a b
-- @
--
-- The process of deriving an SOP-instance:
-- 1. Derive @GHC.Generic@.
-- 2. Use that to derive @SOP.Generic@.
-- 3. Use that to derive your instance of choice via @SOP.Generically@.
--
-- @
-- {-# Language DerivingStrategies #-}
-- {-# Language DerivingVia        #-}
--
-- import qualified GHC.Generics as GHC
-- import qualified Generics.SOP as SOP
--
-- -- >> Ok 1 2 == Ok 1 300
-- -- False
-- data Ok = Ok Int Int
--   deriving -- (1)
--   stock GHC.Generic
--
--   deriving SOP.Generic -- (2)
--   via GHC.Generically T
--
--   deriving Eq -- (3)
--   via SOP.Generically T
-- @
newtype Generically a = Generically a

-- | A class of datatypes that have associated metadata.
--
-- It is possible to use the sum-of-products approach to generic programming
-- without metadata. If you need metadata in a function, an additional
-- constraint on this class is in order.
--
-- You typically don't define instances of this class by hand, but
-- rather derive the class instance automatically. See the documentation
-- of 'Generic' for the options.
--
class Generic a => HasDatatypeInfo a where
  -- | Type-level datatype info
  type DatatypeInfoOf a :: T.DatatypeInfo
  type DatatypeInfoOf a = GDatatypeInfoOf a

  -- | Term-level datatype info; by default, the term-level datatype info is produced
  -- from the type-level info.
  --
  datatypeInfo         :: proxy a -> DatatypeInfo (Code a)
  default datatypeInfo :: (GDatatypeInfo a, GCode a ~ Code a) => proxy a -> DatatypeInfo (Code a)
  datatypeInfo = gdatatypeInfo

-- | A generic ('GHC.Generics') implementation of datatype metadata.
--
-- @
-- {-# Language DerivingStrategies #-}
-- {-# Language DerivingVia        #-}
--
-- import qualified GHC.Generics as GHC
-- import qualified Generics.SOP as SOP
-- import Generics.SOP
--
-- data T = ...
--   deriving
--   stock GHC.Generic
--
--   deriving (SOP.Generic, HasDatatypeInfo)
--   via GHC.Generically T
-- @
instance (All SListI (GCode a), GHC.Generic a, GFrom a, GTo a, GDatatypeInfo a) => SOP.HasDatatypeInfo (GHC.Generically a) where
  type DatatypeInfoOf (GHC.Generically a) = GDatatypeInfoOf a

  datatypeInfo :: proxy (GHC.Generically a) -> SOP.DatatypeInfo (SOP.Code (GHC.Generically a))
  datatypeInfo _ = gdatatypeInfo (Proxy :: Proxy a)

-- | Constraint that captures that a datatype is a product type,
-- i.e., a type with a single constructor.
--
-- It also gives access to the code for the arguments of that
-- constructor.
--
-- @since 0.3.1.0
--
type IsProductType (a :: Type) (xs :: [Type]) =
  (Generic a, Code a ~ '[ xs ])

-- | Direct access to the part of the code that is relevant
-- for a product type.
--
-- @since 0.4.0.0
--
type ProductCode (a :: Type) =
  Head (Code a)

-- | Convert from a product type to its product representation.
--
-- @since 0.4.0.0
--
productTypeFrom :: IsProductType a xs => a -> NP I xs
productTypeFrom = unZ . unSOP . from
{-# INLINE productTypeFrom #-}

-- | Convert a product representation to the original type.
--
-- @since 0.4.0.0
--
productTypeTo :: IsProductType a xs => NP I xs -> a
productTypeTo = to . SOP . Z
{-# INLINE productTypeTo #-}

-- | Constraint that captures that a datatype is an enumeration type,
-- i.e., none of the constructors have any arguments.
--
-- @since 0.3.1.0
--
type IsEnumType (a :: Type) =
  (Generic a, All ((~) '[]) (Code a))

-- | Convert from an enum type to its sum representation.
--
-- @since 0.4.0.0
--
enumTypeFrom :: IsEnumType a => a -> NS (K ()) (Code a)
enumTypeFrom = map_NS (const (K ())) . unSOP . from
{-# INLINE enumTypeFrom #-}

-- | Convert a sum representation to ihe original type.
--
enumTypeTo :: IsEnumType a => NS (K ()) (Code a) -> a
enumTypeTo = to . SOP . cmap_NS (Proxy :: Proxy ((~) '[])) (const Nil)
{-# INLINE enumTypeTo #-}

-- | Constraint that captures that a datatype is a single-constructor,
-- single-field datatype. This always holds for newtype-defined types,
-- but it can also be true for data-defined types.
--
-- The constraint also gives access to the type that is wrapped.
--
-- @since 0.3.1.0
--
type IsWrappedType (a :: Type) (x :: Type) =
  (Generic a, Code a ~ '[ '[ x ] ])

-- | Direct access to the part of the code that is relevant
-- for wrapped types and newtypes.
--
-- @since 0.4.0.0
--
type WrappedCode (a :: Type) =
  Head (Head (Code a))

-- | Convert from a wrapped type to its inner type.
--
-- @since 0.4.0.0
--
wrappedTypeFrom :: IsWrappedType a x => a -> x
wrappedTypeFrom = unI . hd . unZ . unSOP . from
{-# INLINE wrappedTypeFrom #-}

-- | Convert a type to a wrapped type.
--
-- @since 0.4.0.0
--
wrappedTypeTo :: IsWrappedType a x => x -> a
wrappedTypeTo = to . SOP . Z . (:* Nil) . I
{-# INLINE wrappedTypeTo #-}

-- | Constraint that captures that a datatype is a newtype.
-- This makes use of the fact that newtypes are always coercible
-- to the type they wrap, whereas datatypes are not.
--
-- @since 0.3.1.0
--
type IsNewtype (a :: Type) (x :: Type) =
  (IsWrappedType a x, Coercible a x)

-- | Convert a newtype to its inner type.
--
-- This is a specialised synonym for 'coerce'.
--
-- @since 0.4.0.0
--
newtypeFrom :: IsNewtype a x => a -> x
newtypeFrom = coerce
{-# INLINE newtypeFrom #-}

-- | Convert a type to a newtype.
--
-- This is a specialised synonym for 'coerce'.
--
-- @since 0.4.0.0
--
newtypeTo :: IsNewtype a x => x -> a
newtypeTo = coerce
{-# INLINE newtypeTo #-}
