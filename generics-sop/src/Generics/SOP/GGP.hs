{-# LANGUAGE EmptyCase, PolyKinds, UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
{-# LANGUAGE StandaloneKindSignatures #-}
-- | Derive @generics-sop@ boilerplate instances from GHC's 'GHC.Generic'.
--
-- The technique being used here is described in the following paper:
--
--   * José Pedro Magalhães and Andres Löh.
--     <http://www.andres-loeh.de/GenericGenericProgramming Generic Generic Programming>.
--     Practical Aspects of Declarative Languages (PADL) 2014.
--
module Generics.SOP.GGP
  ( GCode
  , GFrom
  , GTo
  , GDatatypeInfo
  , GDatatypeInfoOf
  , gfrom
  , gto
  , gdatatypeInfo
  , DeferMkLifted
  ) where

import Data.Proxy (Proxy (..))
import Data.Kind (Type, Constraint)
import GHC.Generics as GHC
import Generics.SOP.NP as SOP
import Generics.SOP.NS as SOP
import Generics.SOP.BasicFunctors as SOP
import qualified Generics.SOP.Type.Metadata as SOP.T
import Generics.SOP.Metadata as SOP
import GHC.Exts (Levity(Lifted))
import GHC.TypeLits (TypeError, ErrorMessage (Text, ShowType, (:<>:), (:$$:)))

type family ToSingleCode (a :: Type -> Type) :: Type
type instance ToSingleCode (K1 _i a) = a

type family ToProductCode (a :: Type -> Type) (xs :: [Type]) :: [Type]
type instance ToProductCode (a :*: b)   xs = ToProductCode a (ToProductCode b xs)
type instance ToProductCode U1          xs = xs
type instance ToProductCode (M1 S _c a) xs = ToSingleCode a ': xs

type family ToSumCode (a :: Type -> Type) (xs :: [[Type]]) :: [[Type]]
type instance ToSumCode (a :+: b)   xs = ToSumCode a (ToSumCode b xs)
type instance ToSumCode V1          xs = xs
type instance ToSumCode (M1 D _c a) xs = ToSumCode a xs
type instance ToSumCode (M1 C _c a) xs = ToProductCode a '[] ': xs

data InfoProxy (c :: Meta) (f :: Type -> Type) (x :: Type) = InfoProxy

type family ToInfo (a :: Type -> Type) :: SOP.T.DatatypeInfo
type instance ToInfo (M1 D (MetaData n m p False) a) =
  SOP.T.ADT m n (ToSumInfo a '[]) (ToStrictnessInfoss a '[])
type instance ToInfo (M1 D (MetaData n m p True) a) =
  SOP.T.Newtype m n (ToSingleConstructorInfo a)

type family ToStrictnessInfoss (a :: Type -> Type) (xss :: [[SOP.T.StrictnessInfo]]) :: [[SOP.T.StrictnessInfo]]
type instance ToStrictnessInfoss (a :+: b)  xss = ToStrictnessInfoss a (ToStrictnessInfoss b xss)
type instance ToStrictnessInfoss V1         xss = xss
type instance ToStrictnessInfoss (M1 C _ a) xss = ToStrictnessInfos a '[] ': xss

type family ToStrictnessInfos (a :: Type -> Type) (xs :: [SOP.T.StrictnessInfo]) :: [SOP.T.StrictnessInfo]
type instance ToStrictnessInfos (a :*: b)  xs = ToStrictnessInfos a (ToStrictnessInfos b xs)
type instance ToStrictnessInfos U1         xs = xs
type instance ToStrictnessInfos (M1 S s a) xs = ToStrictnessInfo s ': xs

type family ToStrictnessInfo (s :: Meta) :: SOP.T.StrictnessInfo
type instance ToStrictnessInfo (MetaSel _ su ss ds) = 'SOP.T.StrictnessInfo su ss ds

type family ToSumInfo (a :: Type -> Type) (xs :: [SOP.T.ConstructorInfo]) :: [SOP.T.ConstructorInfo]
type instance ToSumInfo (a :+: b)  xs = ToSumInfo a (ToSumInfo b xs)
type instance ToSumInfo V1         xs = xs
type instance ToSumInfo (M1 C c a) xs = ToSingleConstructorInfo (M1 C c a) ': xs

type family ToSingleConstructorInfo (a :: Type -> Type) :: SOP.T.ConstructorInfo
type instance ToSingleConstructorInfo (M1 C (MetaCons n PrefixI False) a) =
  SOP.T.Constructor n
type instance ToSingleConstructorInfo (M1 C (MetaCons n (InfixI assoc fix) False) a) =
  SOP.T.Infix n assoc fix
type instance ToSingleConstructorInfo (M1 C (MetaCons n f True) a) =
  SOP.T.Record n (ToProductInfo a '[])

type family ToProductInfo (a :: Type -> Type) (xs :: [SOP.T.FieldInfo]) :: [SOP.T.FieldInfo]
type instance ToProductInfo (a :*: b)  xs = ToProductInfo a (ToProductInfo b xs)
type instance ToProductInfo U1         xs = xs
type instance ToProductInfo (M1 S c a) xs = ToSingleInfo (M1 S c a) ': xs

type family ToSingleInfo (a :: Type -> Type) :: SOP.T.FieldInfo
type instance ToSingleInfo (M1 S (MetaSel (Just n) _su _ss _ds) a) = 'SOP.T.FieldInfo n

class GFieldInfos (a :: Type -> Type) where
  gFieldInfos :: proxy a -> NP FieldInfo xs -> NP FieldInfo (ToProductCode a xs)

instance (GFieldInfos a, GFieldInfos b) => GFieldInfos (a :*: b) where
  gFieldInfos _ xs = gFieldInfos (Proxy :: Proxy a) (gFieldInfos (Proxy :: Proxy b) xs)

instance GFieldInfos U1 where
  gFieldInfos _ xs = xs

instance (Selector c) => GFieldInfos (M1 S c a) where
  gFieldInfos _ xs = FieldInfo (selName p) :* xs
    where
      p :: InfoProxy c a x
      p = InfoProxy

class GSingleFrom (a :: Type -> Type) where
  gSingleFrom :: a x -> ToSingleCode a

instance GSingleFrom (K1 i a) where
  gSingleFrom (K1 a) = a

class GProductFrom (a :: Type -> Type) where
  gProductFrom :: a x -> NP I xs -> NP I (ToProductCode a xs)

instance (GProductFrom a, GProductFrom b) => GProductFrom (a :*: b) where
  gProductFrom (a :*: b) xs = gProductFrom a (gProductFrom b xs)

instance GProductFrom U1 where
  gProductFrom U1 xs = xs

instance GSingleFrom a => GProductFrom (M1 S c a) where
  gProductFrom (M1 a) xs = I (gSingleFrom a) :* xs

class GSingleTo (a :: Type -> Type) where
  gSingleTo :: ToSingleCode a -> a x

instance GSingleTo (K1 i a) where
  gSingleTo = K1

class GProductTo (a :: Type -> Type) where
  gProductTo :: NP I (ToProductCode a xs) -> (a x -> NP I xs -> r) -> r

instance (GProductTo a, GProductTo b) => GProductTo (a :*: b) where
  gProductTo xs k = gProductTo xs (\ a ys -> gProductTo ys (\ b zs -> k (a :*: b) zs))

instance GSingleTo a => GProductTo (M1 S c a) where
  gProductTo (SOP.I a :* xs) k = k (M1 (gSingleTo a)) xs

instance GProductTo U1 where
  gProductTo xs k = k U1 xs

-- This can most certainly be simplified
class GSumFrom (a :: Type -> Type) where
  gSumFrom :: a x -> proxy xss -> SOP I (ToSumCode a xss)
  gSumSkip :: proxy a -> SOP I xss -> SOP I (ToSumCode a xss)

instance GSumFrom V1 where
  gSumFrom x = case x of {}
  gSumSkip _ xss = xss

instance (GSumFrom a, GSumFrom b) => GSumFrom (a :+: b) where
  gSumFrom (L1 a) xss = gSumFrom a (toSumCodeProxy xss) where
    toSumCodeProxy :: proxy xss -> Proxy (ToSumCode b xss)
    toSumCodeProxy _ = Proxy

  gSumFrom (R1 b) xss = gSumSkip (Proxy :: Proxy a) (gSumFrom b xss)

  gSumSkip _ xss = gSumSkip (Proxy :: Proxy a) (gSumSkip (Proxy :: Proxy b) xss)

instance (GSumFrom a) => GSumFrom (M1 D c a) where
  gSumFrom (M1 a) xss = gSumFrom a xss
  gSumSkip _      xss = gSumSkip (Proxy :: Proxy a) xss

instance (GProductFrom a) => GSumFrom (M1 C c a) where
  gSumFrom (M1 a) _    = SOP (Z (gProductFrom a Nil))
  gSumSkip _ (SOP xss) = SOP (S xss)

class GSumTo (a :: Type -> Type) where
  gSumTo :: SOP I (ToSumCode a xss) -> (a x -> r) -> (SOP I xss -> r) -> r

instance GSumTo V1 where
  gSumTo x _ k = k x

instance (GSumTo a, GSumTo b) => GSumTo (a :+: b) where
  gSumTo xss s k = gSumTo xss (s . L1) (\ r -> gSumTo r (s . R1) k)

instance (GProductTo a) => GSumTo (M1 C c a) where
  gSumTo (SOP (Z xs)) s _ = s (M1 (gProductTo xs ((\ x Nil -> x) :: a x -> NP I '[] -> a x)))
  gSumTo (SOP (S xs)) _ k = k (SOP xs)

instance (GSumTo a) => GSumTo (M1 D c a) where
  gSumTo xss s = gSumTo xss (s . M1)

type MkTUnliftedError :: ErrorMessage -> k -> l
type family MkTUnliftedError tfName ty where
  MkTUnliftedError tfName ty = TypeError
    (tfName :<>: 'Text " only supports Lifted Types"
      :$$: 'Text "but " :<>: 'ShowType ty :<>: 'Text " is unlifted"
    )

type DeferMkLifted :: (Type -> Constraint) -> BoxedType levity -> Constraint
type family DeferMkLifted c ty where
  DeferMkLifted @'Lifted c ty = c ty
  DeferMkLifted c ty = MkTUnliftedError ('ShowType c) ty
 

-- | Compute the SOP code of a datatype.
--
-- This requires that 'GHC.Rep' is defined, which in turn requires that
-- the type has a 'GHC.Generic' (from module "GHC.Generics") instance.
--
-- This is the default definition for 'Generics.SOP.Code'.
-- For more info, see 'Generics.SOP.Generic'.
--
type GCode :: BoxedType levity -> [[BoxedType levity]]
type family GCode a where
  GCode @'Lifted a = ToSumCode (GHC.Rep a) '[]
  GCode a = MkTUnliftedError ('Text "GCode") a

-- | Constraint for the class that computes 'gfrom'.
type GFrom :: BoxedType levity -> Constraint
type family GFrom a where
  GFrom @'Lifted a = GSumFrom (GHC.Rep a)
  GFrom a = MkTUnliftedError ('Text "GFrom") a

-- | Constraint for the class that computes 'gto'.
type GTo :: BoxedType levity -> Constraint
type family GTo a where
  GTo @'Lifted a = GSumTo (GHC.Rep a)
  GTo a = MkTUnliftedError ('Text "GTo") a

-- | Constraint for the class that computes 'gdatatypeInfo'.
type GDatatypeInfo a = SOP.T.DemoteDatatypeInfo (GDatatypeInfoOf a) (GCode a)

-- | Compute the datatype info of a datatype.
--
-- @since 0.3.0.0
--
type GDatatypeInfoOf (a :: Type) = ToInfo (GHC.Rep a)

-- | An automatically computed version of 'Generics.SOP.from'.
--
-- This requires that the type being converted has a
-- 'GHC.Generic' (from module "GHC.Generics") instance.
--
-- This is the default definition for 'Generics.SOP.from'.
-- For more info, see 'Generics.SOP.Generic'.
--
gfrom :: (GFrom a, GHC.Generic a) => a -> SOP I (GCode a)
gfrom x = gSumFrom (GHC.from x) (Proxy :: Proxy '[])

-- | An automatically computed version of 'Generics.SOP.to'.
--
-- This requires that the type being converted has a
-- 'GHC.Generic' (from module "GHC.Generics") instance.
--
-- This is the default definition for 'Generics.SOP.to'.
-- For more info, see 'Generics.SOP.Generic'.
--
gto :: forall a. (GTo a, GHC.Generic a) => SOP I (GCode a) -> a
gto x = GHC.to (gSumTo x id ((\y -> case y of {}) :: SOP I '[] -> (GHC.Rep a) x))

-- | An automatically computed version of 'Generics.SOP.datatypeInfo'.
--
-- This requires that the type being converted has a
-- 'GHC.Generic' (from module "GHC.Generics") instance.
--
-- This is the default definition for 'Generics.SOP.datatypeInfo'.
-- For more info, see 'Generics.SOP.HasDatatypeInfo'.
--
gdatatypeInfo :: forall proxy a. (GDatatypeInfo a) => proxy a -> DatatypeInfo (GCode a)
gdatatypeInfo _ = SOP.T.demoteDatatypeInfo (Proxy :: Proxy (GDatatypeInfoOf a))

