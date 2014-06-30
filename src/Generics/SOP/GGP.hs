{-# LANGUAGE UndecidableInstances #-}
module Generics.SOP.GGP where

import Data.Proxy
import GHC.Generics as GHC
import Generics.SOP.NP as SOP
import Generics.SOP.NS as SOP
import Generics.SOP.BasicFunctors as SOP
import Generics.SOP.Metadata as SOP
import Generics.SOP.Sing

type family ToSingleCode (a :: * -> *) :: *
type instance ToSingleCode (K1 i a) = a

type family ToProductCode (a :: * -> *) (xs :: [*]) :: [*]
type instance ToProductCode (a :*: b)  xs = ToProductCode a (ToProductCode b xs)
type instance ToProductCode U1         xs = xs
type instance ToProductCode (M1 S c a) xs = ToSingleCode a ': xs

type family ToSumCode (a :: * -> *) (xs :: [[*]]) :: [[*]]
type instance ToSumCode (a :+: b)  xs = ToSumCode a (ToSumCode b xs)
type instance ToSumCode V1         xs = xs
type instance ToSumCode (M1 D c a) xs = ToSumCode a xs
type instance ToSumCode (M1 C c a) xs = ToProductCode a '[] ': xs

type GCode (a :: *) = ToSumCode (GHC.Rep a) '[]

data InfoProxy (c :: *) (f :: * -> *) (x :: *) = InfoProxy

class GDatatypeInfo (a :: * -> *) where
  gDatatypeInfo :: Proxy a -> DatatypeInfo (ToSumCode a '[])

#if !(MIN_VERSION_base(4,7,0))

-- | 'isNewtype' does not exist in "GHC.Generics" before GHC-7.8.
--
-- The only safe assumption to make is that it always returns 'False'.
--
isNewtype :: Datatype d => t d (f :: * -> *) a -> Bool
isNewtype _ = False

#endif

instance (SingI (ToSumCode a '[]), Datatype c, GConstructorInfos a) => GDatatypeInfo (M1 D c a) where
  gDatatypeInfo _ =
    let adt = ADT     (moduleName p) (datatypeName p)
        ci  = gConstructorInfos (Proxy :: Proxy a) Nil
    in if isNewtype p
       then case isNewtypeShape sing ci of
              Yes c -> Newtype (moduleName p) (datatypeName p) c
              No    -> adt ci
       else adt ci
    where
     p :: InfoProxy c a x
     p = InfoProxy

data IsNewtypeShape (xss :: [[*]]) where
  Yes :: ConstructorInfo '[x] -> IsNewtypeShape '[ '[x] ]
  No  :: IsNewtypeShape xss

isNewtypeShape :: Sing xss -> NP ConstructorInfo xss -> IsNewtypeShape xss
isNewtypeShape SCons (x :* Nil) = go shape x
  where
    go :: Shape xs -> ConstructorInfo xs -> IsNewtypeShape '[ xs ]
    go (ShapeCons ShapeNil) c   = Yes c
    go _                    _   = No
isNewtypeShape _     _          = No

class GConstructorInfos (a :: * -> *) where
  gConstructorInfos :: Proxy a -> NP ConstructorInfo xss -> NP ConstructorInfo (ToSumCode a xss)

instance (GConstructorInfos a, GConstructorInfos b) => GConstructorInfos (a :+: b) where
  gConstructorInfos _ xss = gConstructorInfos (Proxy :: Proxy a) (gConstructorInfos (Proxy :: Proxy b) xss)

instance GConstructorInfos GHC.V1 where
  gConstructorInfos _ xss = xss

instance (Constructor c, GFieldInfos a, SingI (ToProductCode a '[])) => GConstructorInfos (M1 C c a) where
  gConstructorInfos _ xss
    | conIsRecord p = Record      (conName p) (gFieldInfos (Proxy :: Proxy a) Nil) :* xss
    | otherwise     = Constructor (conName p)                                      :* xss
    where
      p :: InfoProxy c a x
      p = InfoProxy

class GFieldInfos (a :: * -> *) where
  gFieldInfos :: Proxy a -> NP FieldInfo xs -> NP FieldInfo (ToProductCode a xs)

instance (GFieldInfos a, GFieldInfos b) => GFieldInfos (a :*: b) where
  gFieldInfos _ xs = gFieldInfos (Proxy :: Proxy a) (gFieldInfos (Proxy :: Proxy b) xs)

instance GFieldInfos U1 where
  gFieldInfos _ xs = xs

instance (Selector c) => GFieldInfos (M1 S c a) where
  gFieldInfos _ xs = FieldInfo (selName p) :* xs
    where
      p :: InfoProxy c a x
      p = InfoProxy

class GSingleFrom (a :: * -> *) where
  gSingleFrom :: a x -> ToSingleCode a

instance GSingleFrom (K1 i a) where
  gSingleFrom (K1 a) = a

class GProductFrom (a :: * -> *) where
  gProductFrom :: a x -> NP I xs -> NP I (ToProductCode a xs)

instance (GProductFrom a, GProductFrom b) => GProductFrom (a :*: b) where
  gProductFrom (a :*: b) xs = gProductFrom a (gProductFrom b xs)

instance GProductFrom U1 where
  gProductFrom U1 xs = xs

instance GSingleFrom a => GProductFrom (M1 S c a) where
  gProductFrom (M1 a) xs = I (gSingleFrom a) :* xs

class GSingleTo (a :: * -> *) where
  gSingleTo :: ToSingleCode a -> a x

instance GSingleTo (K1 i a) where
  gSingleTo a = K1 a

class GProductTo (a :: * -> *) where
  gProductTo :: NP I (ToProductCode a xs) -> (a x -> NP I xs -> r) -> r

instance (GProductTo a, GProductTo b) => GProductTo (a :*: b) where
  gProductTo xs k = gProductTo xs (\ a ys -> gProductTo ys (\ b zs -> k (a :*: b) zs))

instance GSingleTo a => GProductTo (M1 S c a) where
  gProductTo (SOP.I a :* xs) k = k (M1 (gSingleTo a)) xs
  gProductTo _               _ = error "inaccessible"

instance GProductTo U1 where
  gProductTo xs k = k U1 xs

-- This can most certainly be simplified
class GSumFrom (a :: * -> *) where
  gSumFrom :: a x -> SOP I xss -> SOP I (ToSumCode a xss)
  gSumSkip :: Proxy a -> SOP I xss -> SOP I (ToSumCode a xss)

instance (GSumFrom a, GSumFrom b) => GSumFrom (a :+: b) where
  gSumFrom (L1 a) xss = gSumFrom a (gSumSkip (Proxy :: Proxy b) xss)
  gSumFrom (R1 b) xss = gSumSkip (Proxy :: Proxy a) (gSumFrom b xss)

  gSumSkip _ xss = gSumSkip (Proxy :: Proxy a) (gSumSkip (Proxy :: Proxy b) xss)

instance (GSumFrom a) => GSumFrom (M1 D c a) where
  gSumFrom (M1 a) xss = gSumFrom a xss
  gSumSkip _      xss = gSumSkip (Proxy :: Proxy a) xss

instance (GProductFrom a) => GSumFrom (M1 C c a) where
  gSumFrom (M1 a) _    = SOP (Z (gProductFrom a Nil))
  gSumSkip _ (SOP xss) = SOP (S xss)

class GSumTo (a :: * -> *) where
  gSumTo :: SOP I (ToSumCode a xss) -> (a x -> r) -> (SOP I xss -> r) -> r

instance (GSumTo a, GSumTo b) => GSumTo (a :+: b) where
  gSumTo xss s k = gSumTo xss (s . L1) (\ r -> gSumTo r (s . R1) k)

instance (GProductTo a) => GSumTo (M1 C c a) where
  gSumTo (SOP (Z xs)) s _ = s (M1 (gProductTo xs ((\ x Nil -> x) :: a x -> NP I '[] -> a x)))
  gSumTo (SOP (S xs)) _ k = k (SOP xs)

instance (GSumTo a) => GSumTo (M1 D c a) where
  gSumTo xss s k = gSumTo xss (s . M1) k

gfrom :: (GSumFrom (GHC.Rep a), GHC.Generic a) => a -> SOP I (GCode a)
gfrom x = gSumFrom (GHC.from x) (error "gfrom: internal error" :: SOP.SOP SOP.I '[])

gto :: forall a. (GSumTo (GHC.Rep a), GHC.Generic a) => SOP I (GCode a) -> a
gto x = GHC.to (gSumTo x id ((\ _ -> error "inaccessible") :: SOP I '[] -> (GHC.Rep a) x))

gdatatypeInfo :: forall a. (GDatatypeInfo (GHC.Rep a)) => Proxy a -> DatatypeInfo (GCode a)
gdatatypeInfo _ = gDatatypeInfo (Proxy :: Proxy (GHC.Rep a))

{-
type instance Code a = GCode a

instance (GSumFrom (GHC.Rep a), GSumTo (GHC.Rep a), SingI (Code a), GHC.Generic a) => SOP.Generic a where
  from = gfrom
  to   = gto

instance (GDatatypeInfo (GHC.Rep a)) => HasDatatypeInfo a where
  datatypeInfo _ = gDatatypeInfo (Proxy :: Proxy (GHC.Rep a))
-}
