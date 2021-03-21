{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
-- | Codes and interpretations
module Generics.SOP.Universe1 where

import Data.Kind (Type)
import Data.Coerce (Coercible, coerce)
import Data.Proxy
import GHC.TypeLits

import Generics.SOP.BasicFunctors
import Generics.SOP.Constraint
import Generics.SOP.NP
import Generics.SOP.NS
import qualified Generics.SOP.Universe as U

-- | The (generic) representation of a datatype.
--
-- A datatype is isomorphic to the sum-of-products of its code.
-- The isomorphism is witnessed by 'from' and 'to' from the
-- 'Generic' class.
--
type Rep1 f a = SOP (AppTo1 a) (Code1 f)

-- | Class of representable type constructors.
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
class (All SListI (Code1 f)) => Generic1 (f :: k -> Type) where
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
  type Code1 f :: [[k -> Type]]

  -- | Converts from a value to its structural representation.
  from1        :: f a -> Rep1 f a

  -- | Converts from a structural representation back to the
  -- original value.
  to1          :: Rep1 f a -> f a

-- | Constraint that captures that a datatype is a product type,
-- i.e., a type with a single constructor.
--
-- It also gives access to the code for the arguments of that
-- constructor.
--
type IsProductType1 (f :: k -> Type) (xs :: [k -> Type]) =
  (Generic1 f, Code1 f ~ '[ xs ])

-- | Direct access to the part of the code that is relevant
-- for a product type.
--
type ProductCode1 (f :: k -> Type) =
  Head (Code1 f)

-- | Convert from a product type to its product representation.
--
productTypeFrom1 :: IsProductType1 f xs => f a -> NP (AppTo1 a) xs
productTypeFrom1 = unZ . unSOP . from1
{-# INLINE productTypeFrom1 #-}

-- | Convert a product representation to the original type.
--
productTypeTo1 :: IsProductType1 f xs => NP (AppTo1 a) xs -> f a
productTypeTo1 = to1 . SOP . Z
{-# INLINE productTypeTo1 #-}

-- | Constraint that captures that a datatype is an enumeration type,
-- i.e., none of the constructors have any arguments.
--
type IsEnumType1 (f :: k -> Type) =
  (Generic1 f, All ((~) '[]) (Code1 f))

-- | Convert from an enum type to its sum representation.
--
enumTypeFrom1 :: IsEnumType1 f => f a -> NS (K ()) (Code1 f)
enumTypeFrom1 = map_NS (const (K ())) . unSOP . from1
{-# INLINE enumTypeFrom1 #-}

-- | Convert a sum representation to ihe original type.
--
enumTypeTo1 :: IsEnumType1 f => NS (K ()) (Code1 f) -> f a
enumTypeTo1 = to1 . SOP . cmap_NS (Proxy :: Proxy ((~) '[])) (const Nil)
{-# INLINE enumTypeTo1 #-}

-- | Constraint that captures that a datatype is a single-constructor,
-- single-field datatype. This always holds for newtype-defined types,
-- but it can also be true for data-defined types.
--
-- The constraint also gives access to the type that is wrapped.
--
type IsWrappedType1 (f :: k -> Type) (x :: k -> Type) =
  (Generic1 f, Code1 f ~ '[ '[ x ] ])

-- | Direct access to the part of the code that is relevant
-- for wrapped types and newtypes.
--
type WrappedCode1 (f :: k -> Type) =
  Head (Head (Code1 f))

-- | Convert from a wrapped type to its inner type.
--
wrappedTypeFrom1 :: IsWrappedType1 f x => f a -> x a
wrappedTypeFrom1 = unAppTo1 . hd . unZ . unSOP . from1
{-# INLINE wrappedTypeFrom1 #-}

-- | Convert a type to a wrapped type.
--
wrappedTypeTo1 :: IsWrappedType1 f x => x a -> f a
wrappedTypeTo1 = to1 . SOP . Z . (:* Nil) . AppTo1
{-# INLINE wrappedTypeTo1 #-}

-- | Constraint that captures that a datatype is a newtype.
-- This makes use of the fact that newtypes are always coercible
-- to the type they wrap, whereas datatypes are not.
--
type IsNewtype1 (f :: k -> Type) (x :: k -> Type) =
  (IsWrappedType1 f x, Coercible f x)

-- | Convert a newtype to its inner type.
--
-- This is a specialised synonym for 'coerce'.
--
newtypeFrom1 :: IsNewtype1 f x => f a -> x a
newtypeFrom1 = coerce
{-# INLINE newtypeFrom1 #-}

-- | Convert a type to a newtype.
--
-- This is a specialised synonym for 'coerce'.
--
newtypeTo1 :: IsNewtype1 f x => x a -> f a
newtypeTo1 = coerce
{-# INLINE newtypeTo1 #-}



-- | Abstract one type out of another.
--
-- This function is based on certain heuristics, to find a balance
-- between practicality and completeness.
--
-- We could in principle base it on full SKI, but it currently is not.
-- The reason is that it is much easier to give useful instances to
-- special instances of the S combinator than to the S combinator in
-- general.
--
-- The function 'Abstract' should satisfy the following properties:
--
-- 1. @Coercible (AppTo1 a (Abstract a b)) b@.
--
-- 2. @a@ should not appear free in @Abstract a b@.
--
-- It is property 2 that is currently not satisfied in all cases.
--
-- TODO: Explain or improve @Protect@.
--
type family Abstract (a :: k1) (b :: k2) :: k1 -> k2 where
  Abstract a (Protect b) = K b
  Abstract a a = I
  Abstract a (a b) =
    IfFreeIn a b (CannotAbstractError a (a b)) (AppTo1 b)
  Abstract a (f a) =
    IfFreeIn a f (CannotAbstractError a (f a)) f
  Abstract a (f b) =
    IfFreeIn a f (CannotAbstractError a (f b))
      (IfFreeIn a b (f :.: Abstract a b) (K (f b)))
  Abstract a b =
    IfFreeIn a b (CannotAbstractError a b) (K b)
  Abstract a b =
    IfFreeIn a b (CannotAbstractError a b) (K1 b)
  Abstract a b =
    CannotAbstractError a b

type family CannotAbstractError (a :: k1) (b :: k2) where
  CannotAbstractError a b =
    TypeError ('Text "Cannot abstract " ':<>: 'ShowType a ':<>: 'Text " from " ':<>: 'ShowType b)

type family IfFreeIn (a :: k1) (b :: k2) (t :: k3) (e :: k3) :: k3 where
  IfFreeIn a a     t _ = t
  IfFreeIn a (f g) t e = IfFreeIn a f t (IfFreeIn a g t e)
  IfFreeIn a b     _ e = e

-- | Deriving via adapter to derive 'Generic1' instance from 'Generic' instance.
newtype ToGeneric1 f a = ToGeneric1 (f a)

class
  ( AllZip (AllZip (LiftedCoercible I (AppTo1 a))) (U.Code (f a)) (Code1 (ToGeneric1 f))
  , AllZip (AllZip (LiftedCoercible (AppTo1 a) I)) (Code1 (ToGeneric1 f)) (U.Code (f a))
  ) => CoerceFromToAux1 f a where
  coerceFrom1 :: Proxy f -> SOP I (U.Code (f a)) -> SOP (AppTo1 a) (Code1 (ToGeneric1 f))
  coerceTo1 :: Proxy f -> SOP (AppTo1 a) (Code1 (ToGeneric1 f)) -> SOP I (U.Code (f a))

instance
  ( AllZip (AllZip (LiftedCoercible I (AppTo1 a))) (U.Code (f a)) (Code1 (ToGeneric1 f))
  , AllZip (AllZip (LiftedCoercible (AppTo1 a) I)) (Code1 (ToGeneric1 f)) (U.Code (f a))
  ) => CoerceFromToAux1 f a where
  coerceFrom1 :: Proxy f -> SOP I (U.Code (f a)) -> SOP (AppTo1 a) (Code1 (ToGeneric1 f))
  coerceFrom1 _ = coerce_SOP
  coerceTo1 :: Proxy f -> SOP (AppTo1 a) (Code1 (ToGeneric1 f)) -> SOP I (U.Code (f a))
  coerceTo1 _ = coerce_SOP

instance
  ( forall a . U.Generic (f a)
  , All (All Top) (Code1 (ToGeneric1 f))
  , forall a . CoerceFromToAux1 f a
  ) => Generic1 (ToGeneric1 f) where

  type Code1 (ToGeneric1 f) = Map (MapFun (AbsFun (Hidden 0))) (U.Code (f (Hidden 0)))

  from1 :: forall a . ToGeneric1 f a -> SOP (AppTo1 a) (Code1 (ToGeneric1 f))
  from1 (ToGeneric1 x) =
    coerceFrom1 (Proxy @f) (U.from x)

  to1 :: forall a . SOP (AppTo1 a) (Code1 (ToGeneric1 f)) -> ToGeneric1 f a
  to1 x =
    ToGeneric1 (U.to (coerceTo1 (Proxy @f) x))

newtype Protect a = Protect a

data family Hidden (n :: Nat) :: k

type family Map (f :: Fun a b) (xs :: [a]) :: [b] where
  Map _ '[] = '[]
  Map f (x : xs) = Run f x : Map f xs

data Fun :: Type -> Type -> Type where
  MapFun  :: Fun a b -> Fun [a] [b]
  AbsFun  :: b -> Fun a (b -> a)
  CompFun :: Fun b c -> Fun a b -> Fun a c

type family Run (f :: Fun a b) (x :: a) :: b where
  Run (MapFun f)    x = Map f x
  Run (AbsFun f)    x = Abstract f x
  Run (CompFun f g) x = Run f (Run g x)

