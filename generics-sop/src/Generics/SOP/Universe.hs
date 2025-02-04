{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
-- | Codes and interpretations
module Generics.SOP.Universe where

import Data.Kind (Type)
import Data.Coerce (Coercible, coerce)
import Data.Proxy
import qualified GHC.Generics as GHC

import Generics.SOP.BasicFunctors
import Generics.SOP.Constraint
import Generics.SOP.NP
import Generics.SOP.NS
import Generics.SOP.GGP
import Generics.SOP.Metadata
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
-- You typically don't define instances of this class by hand, but
-- rather derive the class instance automatically.
--
-- /Option 1:/ Derive via the built-in GHC-generics. For this, you
-- need to use the @DeriveGeneric@ extension to first derive an
-- instance of the 'GHC.Generics.Generic' class from module "GHC.Generics".
-- With this, you can then give an empty instance for 'Generic', and
-- the default definitions will just work. The pattern looks as
-- follows:
--
-- @
-- import qualified "GHC.Generics" as GHC
-- import "Generics.SOP"
--
-- ...
--
-- data T = ... deriving (GHC.'GHC.Generics.Generic', ...)
--
-- instance 'Generic' T -- empty
-- instance 'HasDatatypeInfo' T -- empty, if you want/need metadata
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

-- | Constraint that captures that a datatype is a (simple) sum type,
-- i.e., a type with some number of constructors, each of which 
-- has a single argument.
--
-- It also gives access to the list of types which make up the union.
--
-- @since 0.5.2.0
--
type IsSumType (a :: Type) (xs :: [Type]) =
  (Generic a, AllZip IsSingletonOf xs (Code a))

-- | Direct access to the list of types that makes up a sum type.
--
-- @since 0.5.2.0
--
type SumCode (a :: Type) = Heads (Code a)

-- | Convert from a sum type to its sum representation.
--
-- @since 0.5.2.0
--
sumTypeTo :: IsSumType a xs => a -> NS I xs
sumTypeTo = go . unSOP . from
  where
    go :: AllZip IsSingletonOf xs xss => NS (NP I) xss -> NS I xs
    go (Z (x :* Nil)) = Z x
    go (S xss) = S $ go xss
{-# INLINE sumTypeTo #-}

-- | Convert a sum representation to the original type.
--
-- @since 0.5.2.0
--
sumTypeFrom :: IsSumType a xs => NS I xs -> a
sumTypeFrom = to . SOP . go
  where
    go :: AllZip IsSingletonOf xs xss => NS I xs -> NS (NP I) xss
    go (Z x) = Z (x :* Nil)
    go (S xss) = S $ go xss
{-# INLINE sumTypeFrom #-}

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

-- | Convert a enum representation to ihe original type.
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
