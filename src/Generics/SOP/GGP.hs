{-# LANGUAGE PolyKinds, UndecidableInstances #-}
#if __GLASGOW_HASKELL__ >= 780
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
#endif
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
#if MIN_VERSION_base(4,9,0)
  , GDatatypeInfoOf
#endif
  , gfrom
  , gto
  , gdatatypeInfo
  ) where

import Data.Proxy
import GHC.Generics as GHC
import Generics.SOP.NP as SOP
import Generics.SOP.NS as SOP
import Generics.SOP.BasicFunctors as SOP hiding ((:*:))
#if !(MIN_VERSION_base(4,9,0))
import Generics.SOP.Constraint as SOP
#endif
#if MIN_VERSION_base(4,9,0)
import qualified Generics.SOP.Type.Metadata as SOP.T
#endif
import Generics.SOP.Metadata as SOP
#if !(MIN_VERSION_base(4,9,0))
import Generics.SOP.Sing
#endif

type family ToSingleCode (a :: * -> *) :: *
type instance ToSingleCode (K1 _i a) = a

type family ToProductCode (a :: * -> *) (xs :: [*]) :: [*]
type instance ToProductCode (a :*: b)   xs = ToProductCode a (ToProductCode b xs)
type instance ToProductCode U1          xs = xs
type instance ToProductCode (M1 S _c a) xs = ToSingleCode a ': xs

type family ToSumCode (a :: * -> *) (xs :: [[*]]) :: [[*]]
type instance ToSumCode (a :+: b)   xs = ToSumCode a (ToSumCode b xs)
type instance ToSumCode V1          xs = xs
type instance ToSumCode (M1 D _c a) xs = ToSumCode a xs
type instance ToSumCode (M1 C _c a) xs = ToProductCode a '[] ': xs

#if MIN_VERSION_base(4,9,0)
data InfoProxy (c :: Meta) (f :: * -> *) (x :: *) = InfoProxy
#else
data InfoProxy (c :: *) (f :: * -> *) (x :: *) = InfoProxy
#endif

#if !(MIN_VERSION_base(4,9,0))
class GDatatypeInfo' (a :: * -> *) where
  gDatatypeInfo' :: proxy a -> DatatypeInfo (ToSumCode a '[])

#if !(MIN_VERSION_base(4,7,0))

-- | 'isNewtype' does not exist in "GHC.Generics" before GHC-7.8.
--
-- The only safe assumption to make is that it always returns 'False'.
--
isNewtype :: Datatype d => t d (f :: * -> *) a -> Bool
isNewtype _ = False
{-# INLINE isNewtype #-}

#endif

instance (All SListI (ToSumCode a '[]), Datatype c, GConstructorInfos a) => GDatatypeInfo' (M1 D c a) where
  gDatatypeInfo' _ =
    let adt = ADT     (GHC.moduleName p) (GHC.datatypeName p)
        ci  = gConstructorInfos (Proxy :: Proxy a) Nil
    in if isNewtype p
       then case isNewtypeShape ci of
              NewYes c -> Newtype (GHC.moduleName p) (GHC.datatypeName p) c
              NewNo    -> adt ci -- should not happen
       else adt ci
    where
     p :: InfoProxy c a x
     p = InfoProxy
  {-# INLINE gDatatypeInfo' #-}

data IsNewtypeShape (xss :: [[*]]) where
  NewYes :: ConstructorInfo '[x] -> IsNewtypeShape '[ '[x] ]
  NewNo  :: IsNewtypeShape xss

isNewtypeShape :: All SListI xss => NP ConstructorInfo xss -> IsNewtypeShape xss
isNewtypeShape (x :* Nil) = go shape x
  where
    go :: Shape xs -> ConstructorInfo xs -> IsNewtypeShape '[ xs ]
    go (ShapeCons ShapeNil) c   = NewYes c
    go _                    _   = NewNo
isNewtypeShape _          = NewNo
{-# INLINE isNewtypeShape #-}

class GConstructorInfos (a :: * -> *) where
  gConstructorInfos :: proxy a -> NP ConstructorInfo xss -> NP ConstructorInfo (ToSumCode a xss)
  {-# INLINE gConstructorInfos #-}

instance (GConstructorInfos a, GConstructorInfos b) => GConstructorInfos (a :+: b) where
  gConstructorInfos _ xss = gConstructorInfos (Proxy :: Proxy a) (gConstructorInfos (Proxy :: Proxy b) xss)
  {-# INLINE gConstructorInfos #-}

instance GConstructorInfos GHC.V1 where
  gConstructorInfos _ xss = xss
  {-# INLINE gConstructorInfos #-}

instance (Constructor c, GFieldInfos a, SListI (ToProductCode a '[])) => GConstructorInfos (M1 C c a) where
  gConstructorInfos _ xss
    | conIsRecord p = Record (conName p) (gFieldInfos (Proxy :: Proxy a) Nil) :* xss
    | otherwise     = case conFixity p of
        Prefix        -> Constructor (conName p) :* xss
        GHC.Infix a f -> case (shape :: Shape (ToProductCode a '[])) of
          ShapeCons (ShapeCons ShapeNil) -> SOP.Infix (conName p) a f :* xss
          _                              -> Constructor (conName p) :* xss -- should not happen
    where
      p :: InfoProxy c a x
      p = InfoProxy
  {-# INLINE gConstructorInfos #-}
#endif

#if MIN_VERSION_base(4,9,0)
type family ToInfo (a :: * -> *) :: SOP.T.DatatypeInfo
type instance ToInfo (M1 D (MetaData n m p False) a) =
  SOP.T.ADT m n (ToSumInfo a '[])
type instance ToInfo (M1 D (MetaData n m p True) a) =
  SOP.T.Newtype m n (ToSingleConstructorInfo a)

type family ToSumInfo (a :: * -> *) (xs :: [SOP.T.ConstructorInfo]) :: [SOP.T.ConstructorInfo]
type instance ToSumInfo (a :+: b)  xs = ToSumInfo a (ToSumInfo b xs)
type instance ToSumInfo V1         xs = xs
type instance ToSumInfo (M1 C c a) xs = ToSingleConstructorInfo (M1 C c a) ': xs

type family ToSingleConstructorInfo (a :: * -> *) :: SOP.T.ConstructorInfo
type instance ToSingleConstructorInfo (M1 C (MetaCons n PrefixI False) a) =
  SOP.T.Constructor n
type instance ToSingleConstructorInfo (M1 C (MetaCons n (InfixI assoc fix) False) a) =
  SOP.T.Infix n assoc fix
type instance ToSingleConstructorInfo (M1 C (MetaCons n f True) a) =
  SOP.T.Record n (ToProductInfo a '[])

type family ToProductInfo (a :: * -> *) (xs :: [SOP.T.FieldInfo]) :: [SOP.T.FieldInfo]
type instance ToProductInfo (a :*: b)  xs = ToProductInfo a (ToProductInfo b xs)
type instance ToProductInfo U1         xs = xs
type instance ToProductInfo (M1 S c a) xs = ToSingleInfo (M1 S c a) ': xs

type family ToSingleInfo (a :: * -> *) :: SOP.T.FieldInfo
type instance ToSingleInfo (M1 S (MetaSel (Just n) _su _ss _ds) a) = 'SOP.T.FieldInfo n
#endif

class GFieldInfos (a :: * -> *) where
  gFieldInfos :: proxy a -> NP FieldInfo xs -> NP FieldInfo (ToProductCode a xs)

instance (GFieldInfos a, GFieldInfos b) => GFieldInfos (a :*: b) where
  gFieldInfos _ xs = gFieldInfos (Proxy :: Proxy a) (gFieldInfos (Proxy :: Proxy b) xs)
  {-# INLINE gFieldInfos #-}

instance GFieldInfos U1 where
  gFieldInfos _ xs = xs
  {-# INLINE gFieldInfos #-}

instance (Selector c) => GFieldInfos (M1 S c a) where
  gFieldInfos _ xs = FieldInfo (selName p) :* xs
    where
      p :: InfoProxy c a x
      p = InfoProxy
  {-# INLINE gFieldInfos #-}

class GSingleFrom (a :: * -> *) where
  gSingleFrom :: a x -> ToSingleCode a

instance GSingleFrom (K1 i a) where
  gSingleFrom (K1 a) = a
  {-# INLINE gSingleFrom #-}

class GProductFrom (a :: * -> *) where
  gProductFrom :: a x -> NP I xs -> NP I (ToProductCode a xs)

instance (GProductFrom a, GProductFrom b) => GProductFrom (a :*: b) where
  gProductFrom (a :*: b) xs = gProductFrom a (gProductFrom b xs)
  {-# INLINE gProductFrom #-}

instance GProductFrom U1 where
  gProductFrom U1 xs = xs
  {-# INLINE gProductFrom #-}

instance GSingleFrom a => GProductFrom (M1 S c a) where
  gProductFrom (M1 a) xs = I (gSingleFrom a) :* xs
  {-# INLINE gProductFrom #-}

class GSingleTo (a :: * -> *) where
  gSingleTo :: ToSingleCode a -> a x

instance GSingleTo (K1 i a) where
  gSingleTo a = K1 a
  {-# INLINE gSingleTo #-}

class GProductTo (a :: * -> *) where
  gProductTo :: NP I (ToProductCode a xs) -> (a x -> NP I xs -> r) -> r

instance (GProductTo a, GProductTo b) => GProductTo (a :*: b) where
  gProductTo xs k = gProductTo xs (\ a ys -> gProductTo ys (\ b zs -> k (a :*: b) zs))
  {-# INLINE gProductTo #-}

instance GSingleTo a => GProductTo (M1 S c a) where
  gProductTo (SOP.I a :* xs) k = k (M1 (gSingleTo a)) xs
#if __GLASGOW_HASKELL__ < 800
  gProductTo _               _ = error "inaccessible"
#endif
  {-# INLINE gProductTo #-}

instance GProductTo U1 where
  gProductTo xs k = k U1 xs
  {-# INLINE gProductTo #-}

-- This can most certainly be simplified
class GSumFrom (a :: * -> *) where
  gSumFrom :: a x -> SOP I xss -> SOP I (ToSumCode a xss)
  gSumSkip :: proxy a -> SOP I xss -> SOP I (ToSumCode a xss)

instance (GSumFrom a, GSumFrom b) => GSumFrom (a :+: b) where
  gSumFrom (L1 a) xss = gSumFrom a (gSumSkip (Proxy :: Proxy b) xss)
  gSumFrom (R1 b) xss = gSumSkip (Proxy :: Proxy a) (gSumFrom b xss)

  gSumSkip _ xss = gSumSkip (Proxy :: Proxy a) (gSumSkip (Proxy :: Proxy b) xss)
  {-# INLINE gSumFrom #-}
  {-# INLINE gSumSkip #-}

instance (GSumFrom a) => GSumFrom (M1 D c a) where
  gSumFrom (M1 a) xss = gSumFrom a xss
  gSumSkip _      xss = gSumSkip (Proxy :: Proxy a) xss
  {-# INLINE gSumFrom #-}
  {-# INLINE gSumSkip #-}

instance (GProductFrom a) => GSumFrom (M1 C c a) where
  gSumFrom (M1 a) _    = SOP (Z (gProductFrom a Nil))
  gSumSkip _ (SOP xss) = SOP (S xss)
  {-# INLINE gSumFrom #-}
  {-# INLINE gSumSkip #-}

class GSumTo (a :: * -> *) where
  gSumTo :: SOP I (ToSumCode a xss) -> (a x -> r) -> (SOP I xss -> r) -> r

instance (GSumTo a, GSumTo b) => GSumTo (a :+: b) where
  gSumTo xss s k = gSumTo xss (s . L1) (\ r -> gSumTo r (s . R1) k)
  {-# INLINE gSumTo #-}

instance (GProductTo a) => GSumTo (M1 C c a) where
  gSumTo (SOP (Z xs)) s _ = s (M1 (gProductTo xs ((\ x Nil -> x) :: a x -> NP I '[] -> a x)))
  gSumTo (SOP (S xs)) _ k = k (SOP xs)
  {-# INLINE gSumTo #-}

instance (GSumTo a) => GSumTo (M1 D c a) where
  gSumTo xss s k = gSumTo xss (s . M1) k
  {-# INLINE gSumTo #-}

-- | Compute the SOP code of a datatype.
--
-- This requires that 'GHC.Rep' is defined, which in turn requires that
-- the type has a 'GHC.Generic' (from module "GHC.Generics") instance.
--
-- This is the default definition for 'Generics.SOP.Code'.
-- For more info, see 'Generics.SOP.Generic'.
--
type GCode (a :: *) = ToSumCode (GHC.Rep a) '[]

-- | Constraint for the class that computes 'gfrom'.
type GFrom a = GSumFrom (GHC.Rep a)

-- | Constraint for the class that computes 'gto'.
type GTo a = GSumTo (GHC.Rep a)

-- | Constraint for the class that computes 'gdatatypeInfo'.
#if MIN_VERSION_base(4,9,0)
type GDatatypeInfo a = SOP.T.DemoteDatatypeInfo (GDatatypeInfoOf a) (GCode a)
#else
type GDatatypeInfo a = GDatatypeInfo' (GHC.Rep a)
#endif

#if MIN_VERSION_base(4,9,0)
-- | Compute the datatype info of a datatype.
--
-- @since 0.3.0.0
--
type GDatatypeInfoOf (a :: *) = ToInfo (GHC.Rep a)
#endif

-- | An automatically computed version of 'Generics.SOP.from'.
--
-- This requires that the type being converted has a
-- 'GHC.Generic' (from module "GHC.Generics") instance.
--
-- This is the default definition for 'Generics.SOP.from'.
-- For more info, see 'Generics.SOP.Generic'.
--
gfrom :: (GFrom a, GHC.Generic a) => a -> SOP I (GCode a)
gfrom x = gSumFrom (GHC.from x) (error "gfrom: internal error" :: SOP.SOP SOP.I '[])
{-# INLINE gfrom #-}

-- | An automatically computed version of 'Generics.SOP.to'.
--
-- This requires that the type being converted has a
-- 'GHC.Generic' (from module "GHC.Generics") instance.
--
-- This is the default definition for 'Generics.SOP.to'.
-- For more info, see 'Generics.SOP.Generic'.
--
gto :: forall a. (GTo a, GHC.Generic a) => SOP I (GCode a) -> a
gto x = GHC.to (gSumTo x id ((\ _ -> error "inaccessible") :: SOP I '[] -> (GHC.Rep a) x))
{-# INLINE gto #-}

-- | An automatically computed version of 'Generics.SOP.datatypeInfo'.
--
-- This requires that the type being converted has a
-- 'GHC.Generic' (from module "GHC.Generics") instance.
--
-- This is the default definition for 'Generics.SOP.datatypeInfo'.
-- For more info, see 'Generics.SOP.HasDatatypeInfo'.
--
gdatatypeInfo :: forall proxy a. (GDatatypeInfo a) => proxy a -> DatatypeInfo (GCode a)
#if MIN_VERSION_base(4,9,0)
gdatatypeInfo _ = SOP.T.demoteDatatypeInfo (Proxy :: Proxy (GDatatypeInfoOf a))
#else
gdatatypeInfo _ = gDatatypeInfo' (Proxy :: Proxy (GHC.Rep a))
#endif
{-# INLINE gdatatypeInfo #-}

