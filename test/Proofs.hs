{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fshow-hole-constraints -Wall #-}
-- {-# OPTIONS_GHC -ddump-simpl #-}
-- {-# OPTIONS_GHC -dsuppress-idinfo -dsuppress-module-prefixes #-}
{-# OPTIONS_GHC -O #-}
{-# OPTIONS_GHC -fplugin Test.Inspection.Plugin #-}
-- {-# OPTIONS_GHC -funfolding-creation-threshold=5000 -funfolding-use-threshold=5000 #-}
module Main where

import Data.Monoid (Sum(..), Product(..), (<>))
import Generics.SOP
import Generics.SOP.NP
import Generics.SOP.NS
import Test.Inspection

import Proofs.Metadata
import Proofs.Types

import GHC.Exts

---------------------------------------------------------------------
-- Simple properties

caseSelf_T2 :: T2 a b -> T2 a b
caseSelf_T2 x = case x of T2 a b -> T2 a b

id_T2 :: T2 a b -> T2 a b
id_T2 x = x

inspect $ 'caseSelf_T2 === 'id_T2

caseSelf_NP0 :: NP I '[] -> NP I '[]
caseSelf_NP0 x = case x of Nil -> Nil

id_NP0 :: NP I '[] -> NP I '[]
id_NP0 x = x

-- fails
inspect ('caseSelf_NP0 === 'id_NP0) { expectFail = True }

caseSelf_NP1 :: NP I '[Int] -> NP I '[Int]
caseSelf_NP1 x = case x of y :* Nil -> y :* Nil

id_NP1 :: NP I '[Int] -> NP I '[Int]
id_NP1 x = x

-- fails
inspect ('caseSelf_NP1 === 'id_NP1) { expectFail = True }

---------------------------------------------------------------------
-- Roundtrips, from, to

roundtrip_T2 :: T2 a b -> T2 a b
roundtrip_T2 = to . from

inspect $ 'roundtrip_T2 === 'id_T2

roundtrip_T2' :: T2' a b -> T2' a b
roundtrip_T2' = to . from

id_T2' :: T2' a b -> T2' a b
id_T2' x = x

inspect $ 'roundtrip_T2' === 'id_T2'

roundtrip_Bool :: Bool -> Bool
roundtrip_Bool = to . from

id_Bool :: Bool -> Bool
id_Bool x = x

inspect $ 'roundtrip_Bool === 'id_Bool

roundtrip_Ordering :: Ordering -> Ordering
roundtrip_Ordering = to . from

id_Ordering :: Ordering -> Ordering
id_Ordering x = x

inspect $ 'roundtrip_Ordering === 'id_Ordering

roundtrip_E1 :: E1 -> E1
roundtrip_E1 = to . from

id_E1 :: E1 -> E1
id_E1 x = x

inspect $ 'roundtrip_E1 === 'id_E1

roundtrip_E1' :: E1' -> E1'
roundtrip_E1' = to . from

id_E1' :: E1' -> E1'
id_E1' x = x

inspect $ 'roundtrip_E1' === 'id_E1'

roundtrip_E2 :: E2 -> E2
roundtrip_E2 = to . from

id_E2 :: E2 -> E2
id_E2 x = x

inspect $ 'roundtrip_E2 === 'id_E2

roundtrip_E2' :: E2' -> E2'
roundtrip_E2' = to . from

id_E2' :: E2' -> E2'
id_E2' x = x

inspect $ 'roundtrip_E2' === 'id_E2'

roundtrip_E3 :: E3 -> E3
roundtrip_E3 = to . from

id_E3 :: E3 -> E3
id_E3 x = x

inspect $ 'roundtrip_E3 === 'id_E3

roundtrip_E3' :: E3' -> E3'
roundtrip_E3' = to . from

id_E3' :: E3' -> E3'
id_E3' x = x

inspect $ 'roundtrip_E3' === 'id_E3'

roundtrip_E5 :: E5 -> E5
roundtrip_E5 = to . from

id_E5 :: E5 -> E5
id_E5 x = x

-- needs higher unfolding thresholds
-- needs unfolding-use-threshold of 100, but that makes other
-- equalities break
inspect ('roundtrip_E5 === 'id_E5) { expectFail = True }

roundtrip_E5' :: E5' -> E5'
roundtrip_E5' = to . from

id_E5' :: E5' -> E5'
id_E5' x = x

inspect $ 'roundtrip_E5' === 'id_E5'

doubleRoundtrip_T2 :: forall a b . T2 a b -> T2 a b
doubleRoundtrip_T2 = to . from . (to . from :: T2 a b -> T2 a b)

inspect $ 'doubleRoundtrip_T2 === 'id_T2

doubleRoundtrip_T2' :: forall a b . T2' a b -> T2' a b
doubleRoundtrip_T2' = to . from . (to . from :: T2' a b -> T2' a b)

inspect $ 'doubleRoundtrip_T2' === 'id_T2'

productRoundtrip_T2 :: T2 a b -> T2 a b
productRoundtrip_T2 = productTo . productFrom

inspect $ 'productRoundtrip_T2 === 'id_T2

productRoundtrip_T2' :: T2' a b -> T2' a b
productRoundtrip_T2' = productTo . productFrom

inspect $ 'productRoundtrip_T2' === 'id_T2'

---------------------------------------------------------------------
-- cpure

gmempty :: (IsProductType a xs, All Monoid xs) => a
gmempty =
  productTo (hcpure (Proxy :: Proxy Monoid) (I mempty))
{-# INLINE gmempty #-}

gmempty_T2 :: (Monoid a, Monoid b) => T2 a b
gmempty_T2 = gmempty

mempty_T2 :: (Monoid a, Monoid b) => T2 a b
mempty_T2 =
  T2 mempty mempty

inspect $ 'gmempty_T2 === 'mempty_T2

gmempty_T2' :: (Monoid a, Monoid b) => T2' a b
gmempty_T2' = gmempty

mempty_T2' :: (Monoid a, Monoid b) => T2' a b
mempty_T2' =
  T2' mempty mempty

inspect $ 'gmempty_T2' === 'mempty_T2'

gmempty_T3 :: (Monoid a, Monoid b, Monoid c) => T3 a b c
gmempty_T3 = gmempty

mempty_T3 :: (Monoid a, Monoid b, Monoid c) => T3 a b c
mempty_T3 =
  T3 mempty mempty mempty

inspect $ 'gmempty_T3 === 'mempty_T3

gmempty_T3' :: (Monoid a, Monoid b, Monoid c) => T3' a b c
gmempty_T3' = gmempty

mempty_T3' :: (Monoid a, Monoid b, Monoid c) => T3' a b c
mempty_T3' =
  T3' mempty mempty mempty

inspect $ 'gmempty_T3' === 'mempty_T3'

gmempty_U10 :: (Monoid a) => U10 a
gmempty_U10 = gmempty

mempty_U10 :: (Monoid a) => U10 a
mempty_U10 = U10
    mempty
    mempty
    mempty
    mempty
    mempty
    mempty
    mempty
    mempty
    mempty
    mempty

inspect $ 'gmempty_U10 === 'mempty_U10

gmempty_U10' :: (Monoid a) => U10' a
gmempty_U10' = gmempty

mempty_U10' :: (Monoid a) => U10' a
mempty_U10' = U10'
    mempty
    mempty
    mempty
    mempty
    mempty
    mempty
    mempty
    mempty
    mempty
    mempty

inspect $ 'gmempty_U10' === 'mempty_U10'

gmemptyConcrete_Triple :: (Sum Int, Product Int, [Bool])
gmemptyConcrete_Triple = gmempty

memptyConcrete_Triple :: (Sum Int, Product Int, [Bool])
memptyConcrete_Triple =
  (Sum 0, Product 1, [])

inspect $ 'gmemptyConcrete_Triple === 'memptyConcrete_Triple

gmemptyConcrete_T3 :: T3 (Sum Int) (Product Int) [Bool]
gmemptyConcrete_T3 = gmempty

memptyConcrete_T3 :: T3 (Sum Int) (Product Int) [Bool]
memptyConcrete_T3 =
  T3 (Sum 0) (Product 1) []

inspect $ 'gmemptyConcrete_T3 === 'memptyConcrete_T3

gmemptyConcrete_T3' :: T3' (Sum Int) (Product Int) [Bool]
gmemptyConcrete_T3' = gmempty

memptyConcrete_T3' :: T3' (Sum Int) (Product Int) [Bool]
memptyConcrete_T3' =
  T3' (Sum 0) (Product 1) []

inspect $ 'gmemptyConcrete_T3' === 'memptyConcrete_T3'

---------------------------------------------------------------------
-- Variant of hmap

-- In the library, hmap is defined less directly, via hap.
-- The version here is a direct variant to see if it behaves
-- differently with respect to optimisation.
--
myhmap :: SListI xs => (forall x . f x -> g x) -> NP f xs -> NP g xs
myhmap f =
  apFn $
  cataSList
    (fn (\ Nil -> Nil))
    (\ (Fn rec) -> fn (\ (y :* ys) -> f y :* rec ys))
{-# INLINE myhmap #-}

-- As this is tricky, for completeness, we reproduce the way
-- hmap is defined in the library.
--
libhmap :: SListI xs => (forall x . f x -> g x) -> NP f xs -> NP g xs
libhmap f xs =
  ap_NP (pure_NP (fn f)) xs
{-# INLINE libhmap #-}

libhmap' :: SListI xs => (forall x . f x -> g x) -> NP f xs -> NP g xs
libhmap' = hmap
{-# INLINE libhmap' #-}

libhmapapp :: SListI xs => NP I xs -> NP (K ()) xs
libhmapapp = libhmap (mapIK (const ()))

ghmap :: SListI xs => NP I xs -> NP (K ()) xs
ghmap = myhmap (mapIK (const ()))
{-# INLINE ghmap #-}

ghmap' :: SListI xs => NP I xs -> NP (K ()) xs
ghmap' = hmap (mapIK (const ()))

libhmap_T2 :: NP I '[a,b] -> NP (K ()) '[a,b]
libhmap_T2 = libhmapapp

ghmap_T2 :: NP I '[a,b] -> NP (K ()) '[a,b]
ghmap_T2 = ghmap

ghmap'_T2 :: NP I '[a,b] -> NP (K ()) '[a,b]
ghmap'_T2 = ghmap'

-- Unfortunately, it is quite a bit more difficult than expected
-- to obtain a "hand-written" version of ghmap that has the casts
-- at the correct positions.
--
-- Note that this is not really indicating a performance problem.
--
hmap_T2 :: forall a b . NP I '[a,b] -> NP (K ()) '[a,b]
hmap_T2 = \ x -> case x of
  I (_ :: a') :* ys -> case ys of
    I (_ :: b') :* zs -> case (zs :: NP I '[]) of
      Nil -> (K () :: K () a') :* (K () :: K () b') :* Nil

hmap'_T2 :: forall a b . NP I '[a,b] -> NP (K ()) '[a,b]
hmap'_T2 = \ x -> case x of
  I (_ :: a') :* ys -> case (ys :: NP I '[b]) of
    I (_ :: b') :* zs -> case (zs :: NP I '[]) of
      Nil -> (K () :: K () a') :* (K () :: K () b') :* Nil
  -- The type annotations above are required for the proof
  -- below to go through.
  --
  -- The importans aspect is that in each of the cases, we
  -- explicitly establish whether the 'NP' is empty or not.
  -- So, for example, with partial type signatures,
  -- @NP I (_ : _)@ in the first annotations would also
  -- work.

-- This one should work, because they're actually defined in the same way.
inspect $ 'ghmap'_T2 === 'libhmap_T2

-- In fact, because they're the same function, even the unspecialised
-- versions should be equal.
--
inspect $ 'libhmap' === 'libhmap

-- This one also works, indicating that the lib version actually is simplified
-- properly.
inspect $ 'ghmap'_T2 ==- 'hmap'_T2

-- I'm not entirely sure why this fails.
--
-- The terms are very similar. As far as I can see, they only differ
-- in the way type arguments and cast are applied.
--
-- But (==-) is currently not accepting this.
--
inspect ('ghmap_T2 ==- 'hmap_T2) { expectFail = True }

---------------------------------------------------------------------
-- hcollapse / hmap / hcmap

gnrargs :: (Generic a) => a -> Int
gnrargs =
  sum . hcollapse . hmap (mapIK (const 1)) . from
{-# INLINE gnrargs #-}

gnrargs' :: (Generic a) => a -> SOP (K Int) (Code a)
gnrargs' =
  hmap (mapIK (const 1)) . from
{-# INLINE gnrargs' #-}

gnrargs_Maybe :: Maybe a -> SOP (K Int) (Code (Maybe a))
gnrargs_Maybe = gnrargs'

nrargs_Maybe :: Maybe a -> SOP (K Int) (Code (Maybe a))
nrargs_Maybe Nothing  = SOP (Z Nil)
nrargs_Maybe (Just _) = SOP (S (Z (K 1 :* Nil)))

-- fails for reasons I have no yet been able to determine
-- inspect $ 'gnrargs_Maybe ==- 'nrargs_Maybe

gshow :: (Generic a, All2 Show (Code a)) => a -> String
gshow =
  concat . hcollapse . hcmap (Proxy :: Proxy Show) (mapIK show) . from
{-# INLINE gshow #-}

gproductShow :: (IsProductType a xs, All Show xs) => a -> String
gproductShow =
  concat . hcollapse . hcmap (Proxy :: Proxy Show) (mapIK show) . productFrom
{-# INLINE gproductShow #-}

gshow_T1 :: (Show a) => T1 a -> String
gshow_T1 = gshow

show_T1 :: (Show a) => T1 a -> String
show_T1 (T1 x) = show x

-- fails, due to GGP-conversion for single-constructor single-value datatype being lazy
inspect ('gshow_T1 === 'show_T1) { expectFail = True }

gshow_T1' :: (Show a) => T1' a -> String
gshow_T1' = gshow

show_T1' :: (Show a) => T1' a -> String
show_T1' (T1' x) = show x

inspect $ 'gshow_T1' === 'show_T1'

gproductShow_T1 :: (Show a) => T1 a -> String
gproductShow_T1 = gproductShow

-- fails, due to GGP-conversion for single-constructor single-value datatype being lazy
inspect ('gproductShow_T1 === 'show_T1) { expectFail = True }

gproductShow_T1' :: (Show a) => T1' a -> String
gproductShow_T1' = gproductShow

inspect $ 'gproductShow_T1' === 'show_T1'

gshow_T2 :: (Show a, Show b) => T2 a b -> String
gshow_T2 = gshow

show_T2 :: (Show a, Show b) => T2 a b -> String
show_T2 (T2 x y) = show x ++ show y

inspect $ 'gshow_T2 === 'show_T2

gshow_T2' :: (Show a, Show b) => T2' a b -> String
gshow_T2' = gshow

show_T2' :: (Show a, Show b) => T2' a b -> String
show_T2' (T2' x y) = show x ++ show y

inspect $ 'gshow_T2' === 'show_T2'

gshow_T3 :: (Show a, Show b, Show c) => T3 a b c -> String
gshow_T3 = gshow

show_T3 :: (Show a, Show b, Show c) => T3 a b c -> String
show_T3 (T3 x y z) = show x ++ show y ++ show z

inspect $ 'gshow_T3 === 'show_T3

gshow_T3' :: (Show a, Show b, Show c) => T3' a b c -> String
gshow_T3' = gshow

show_T3' :: (Show a, Show b, Show c) => T3' a b c -> String
show_T3' (T3' x y z) = show x ++ show y ++ show z

inspect $ 'gshow_T3' === 'show_T3'

gshow_U10 :: (Show a) => U10 a -> String
gshow_U10 = gshow

show_U10 :: (Show a) => U10 a -> String
show_U10 (U10 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10) =
     show a1
  ++ show a2
  ++ show a3
  ++ show a4
  ++ show a5
  ++ show a6
  ++ show a7
  ++ show a8
  ++ show a9
  ++ show a10

-- needs higher thresholds
inspect ('gshow_U10 === 'show_U10) { expectFail = True }

gshow_U10' :: (Show a) => U10' a -> String
gshow_U10' = gshow

show_U10' :: (Show a) => U10' a -> String
show_U10' (U10' a1 a2 a3 a4 a5 a6 a7 a8 a9 a10) =
     show a1
  ++ show a2
  ++ show a3
  ++ show a4
  ++ show a5
  ++ show a6
  ++ show a7
  ++ show a8
  ++ show a9
  ++ show a10

inspect $ 'gshow_U10' === 'show_U10'

gshow_E1 :: E1 -> String
gshow_E1 = gshow

show_E1 :: E1 -> String
show_E1 E1_0 = ""

inspect $ 'gshow_E1 === 'show_E1

gshow_E1' :: E1' -> String
gshow_E1' = gshow

show_E1' :: E1' -> String
show_E1' E1'_0 = ""

inspect $ 'gshow_E1' === 'show_E1'

gshow_E2 :: E2 -> String
gshow_E2 = gshow

show_E2 :: E2 -> String
show_E2 !_ = ""

inspect $ 'gshow_E2 === 'show_E2

gshow_E2' :: E2' -> String
gshow_E2' = gshow

show_E2' :: E2' -> String
show_E2' !_ = ""

inspect $ 'gshow_E2' === 'show_E2'

gshow_E3 :: E3 -> String
gshow_E3 = gshow

show_E3 :: E3 -> String
show_E3 !_ = ""

inspect $ 'gshow_E3 === 'show_E3

gshow_E3' :: E3' -> String
gshow_E3' = gshow

show_E3' :: E3' -> String
show_E3' !_ = ""

inspect $ 'gshow_E3' === 'show_E3'

gproductShow_T2 :: (Show a, Show b) => T2 a b -> String
gproductShow_T2 = gproductShow

inspect $ 'gproductShow_T2 === 'show_T2

gproductShow_T2' :: (Show a, Show b) => T2' a b -> String
gproductShow_T2' = gproductShow

inspect $ 'gproductShow_T2' === 'show_T2'

gproductShow_T3 :: (Show a, Show b, Show c) => T3 a b c -> String
gproductShow_T3 = gproductShow

inspect $ 'gproductShow_T3 === 'show_T3

gproductShow_T3' :: (Show a, Show b, Show c) => T3' a b c -> String
gproductShow_T3' = gproductShow

inspect $ 'gproductShow_T3' === 'show_T3'

gproductShow_U10 :: (Show a) => U10 a -> String
gproductShow_U10 = gproductShow

-- needs higher thresholds
inspect ('gproductShow_U10 === 'show_U10) { expectFail = True }

gproductShow_U10' :: (Show a) => U10' a -> String
gproductShow_U10' = gproductShow

inspect $ 'gproductShow_U10' === 'show_U10'

---------------------------------------------------------------------
-- czipWith

type Append = Semigroup
append :: (Append a) => a -> a -> a
append = (<>)
{-# INLINE append #-}

gmappend :: (IsProductType a xs, All Append xs) => a -> a -> a
gmappend =
  \ x y -> productTo (hczipWith (Proxy :: Proxy Append) (mapIII append)
    (productFrom x) (productFrom y))
{-# INLINE gmappend #-}

gmappend_T1 :: (Append a) => T1 a -> T1 a -> T1 a
gmappend_T1 = gmappend

mappend_T1 :: (Append a) => T1 a -> T1 a -> T1 a
mappend_T1 (T1 a0) (T1 b0) = T1 (a0 <> b0)

-- fails, due to GGP-conversion for single-constructor single-value datatype being lazy
inspect ('gmappend_T1 === 'mappend_T1) { expectFail = True }

gmappend_T1' :: (Append a) => T1' a -> T1' a -> T1' a
gmappend_T1' = gmappend

mappend_T1' :: (Append a) => T1' a -> T1' a -> T1' a
mappend_T1' (T1' a0) (T1' b0) = T1' (a0 <> b0)

inspect $ 'gmappend_T1' === 'mappend_T1'

gmappend_T2 :: (Append a, Append b) => T2 a b -> T2 a b -> T2 a b
gmappend_T2 = gmappend

mappend_T2 :: (Append a, Append b) => T2 a b -> T2 a b -> T2 a b
mappend_T2 (T2 a0 a1) (T2 b0 b1) = T2 (a0 <> b0) (a1 <> b1)

inspect $ 'gmappend_T2 === 'mappend_T2

gmappend_T2' :: (Append a, Append b) => T2' a b -> T2' a b -> T2' a b
gmappend_T2' = gmappend

mappend_T2' :: (Append a, Append b) => T2' a b -> T2' a b -> T2' a b
mappend_T2' (T2' a0 a1) (T2' b0 b1) = T2' (a0 <> b0) (a1 <> b1)

inspect $ 'gmappend_T2' === 'mappend_T2'

gmappend_T3 :: (Append a, Append b, Append c) => T3 a b c -> T3 a b c -> T3 a b c
gmappend_T3 = gmappend

mappend_T3 :: (Append a, Append b, Append c) => T3 a b c -> T3 a b c -> T3 a b c
mappend_T3 (T3 a0 a1 a2) (T3 b0 b1 b2) = T3 (a0 <> b0) (a1 <> b1) (a2 <> b2)

inspect $ 'gmappend_T3 === 'mappend_T3

gmappend_T3' :: (Append a, Append b, Append c) => T3' a b c -> T3' a b c -> T3' a b c
gmappend_T3' = gmappend

mappend_T3' :: (Append a, Append b, Append c) => T3' a b c -> T3' a b c -> T3' a b c
mappend_T3' (T3' a0 a1 a2) (T3' b0 b1 b2) = T3' (a0 <> b0) (a1 <> b1) (a2 <> b2)

inspect $ 'gmappend_T3' === 'mappend_T3'

gmappend_U10 :: (Append a) => U10 a -> U10 a -> U10 a
gmappend_U10 = gmappend

mappend_U10 :: (Append a) => U10 a -> U10 a -> U10 a
mappend_U10 (U10 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9) (U10 b0 b1 b2 b3 b4 b5 b6 b7 b8 b9) =
  U10
    (a0 <> b0)
    (a1 <> b1)
    (a2 <> b2)
    (a3 <> b3)
    (a4 <> b4)
    (a5 <> b5)
    (a6 <> b6)
    (a7 <> b7)
    (a8 <> b8)
    (a9 <> b9)

-- needs higher thresholds
inspect ('gmappend_U10 === 'mappend_U10) { expectFail = True }

gmappend_U10' :: (Append a) => U10' a -> U10' a -> U10' a
gmappend_U10' = gmappend

mappend_U10' :: (Append a) => U10' a -> U10' a -> U10' a
mappend_U10' (U10' a0 a1 a2 a3 a4 a5 a6 a7 a8 a9) (U10' b0 b1 b2 b3 b4 b5 b6 b7 b8 b9) =
  U10'
    (a0 <> b0)
    (a1 <> b1)
    (a2 <> b2)
    (a3 <> b3)
    (a4 <> b4)
    (a5 <> b5)
    (a6 <> b6)
    (a7 <> b7)
    (a8 <> b8)
    (a9 <> b9)

inspect $ 'gmappend_U10' === 'mappend_U10'

gmappendConcrete_Triple ::
  (Sum Int, Product Int, [a]) -> (Sum Int, Product Int, [a]) -> (Sum Int, Product Int, [a])
gmappendConcrete_Triple = gmappend

mappendConcrete_Triple ::
  (Sum Int, Product Int, [a]) -> (Sum Int, Product Int, [a]) -> (Sum Int, Product Int, [a])
mappendConcrete_Triple (Sum x1, Product x2, x3) (Sum y1, Product y2, y3) =
  (Sum (x1 + y1), Product (x2 * y2), x3 ++ y3)

inspect $ 'gmappendConcrete_Triple === 'mappendConcrete_Triple

gmappendConcrete_T3 ::
  T3 (Sum Int) (Product Int) [a] -> T3 (Sum Int) (Product Int) [a] -> T3 (Sum Int) (Product Int) [a]
gmappendConcrete_T3 = gmappend

mappendConcrete_T3 ::
  T3 (Sum Int) (Product Int) [a] -> T3 (Sum Int) (Product Int) [a] -> T3 (Sum Int) (Product Int) [a]
mappendConcrete_T3 (T3 (Sum x1) (Product x2) x3) (T3 (Sum y1) (Product y2) y3) =
  T3 (Sum (x1 + y1)) (Product (x2 * y2)) (x3 ++ y3)

inspect $ 'gmappendConcrete_T3 === 'mappendConcrete_T3

---------------------------------------------------------------------
-- Metadata

datatypeNameOf_T1 :: String
datatypeNameOf_T1 =
  datatypeNameOf (Proxy :: Proxy (T1 ()))

name_T1 :: String
name_T1 =
  "T1"

-- fails due to lack of String inlining
inspect ('datatypeNameOf_T1 === 'name_T1) { expectFail = True }

datatypeNameOf_T1' :: String
datatypeNameOf_T1' =
  datatypeNameOf (Proxy :: Proxy (T1' ()))

name_T1' :: String
name_T1' =
  "T1'"

inspect $ 'datatypeNameOf_T1' === 'name_T1'

datatypeNameOf_I10 :: String
datatypeNameOf_I10 =
  datatypeNameOf (Proxy :: Proxy I10)

name_I10 :: String
name_I10 =
  "I10"

-- fails due to lack of String inlining
inspect ('datatypeNameOf_I10 === 'name_I10) { expectFail = True }

datatypeNameOf_I10' :: String
datatypeNameOf_I10' =
  datatypeNameOf (Proxy :: Proxy I10')

name_I10' :: String
name_I10' =
  "I10'"

inspect $ 'datatypeNameOf_I10' === 'name_I10'

gconstructorNames_Bool :: [String]
gconstructorNames_Bool =
  constructorNames (Proxy :: Proxy Bool)

constructorNames_Bool :: [String]
constructorNames_Bool =
  ["False", "True"]

inspect $ 'gconstructorNames_Bool === 'constructorNames_Bool

gconstructorNames_Ordering :: [String]
gconstructorNames_Ordering =
  constructorNames (Proxy :: Proxy Ordering)

constructorNames_Ordering :: [String]
constructorNames_Ordering =
  ["LT", "EQ", "GT"]

inspect $ 'gconstructorNames_Ordering === 'constructorNames_Ordering

gconstructorNames_Maybe :: [String]
gconstructorNames_Maybe =
  constructorNames (Proxy :: Proxy (Maybe Int))

constructorNames_Maybe :: [String]
constructorNames_Maybe =
  ["Nothing", "Just"]

inspect $ 'gconstructorNames_Maybe === 'constructorNames_Maybe

gconstructorNames_I10 :: [String]
gconstructorNames_I10 =
  constructorNames (Proxy :: Proxy I10)

constructorNames_I10 :: [String]
constructorNames_I10 =
  ["I10"]

-- fails due to lack of String inlining
inspect ('gconstructorNames_I10 === 'constructorNames_I10) { expectFail = True }

gconstructorNames_I10' :: [String]
gconstructorNames_I10' =
  constructorNames (Proxy :: Proxy I10')

constructorNames_I10' :: [String]
constructorNames_I10' =
  ["I10'"]

inspect $ 'gconstructorNames_I10' === 'constructorNames_I10'

gtheConstructor_True :: String
gtheConstructor_True =
  theConstructor True

theConstructor_True :: String
theConstructor_True = "True"

inspect $ 'gtheConstructor_True === 'theConstructor_True

gtheConstructor_Bool :: Bool -> String
gtheConstructor_Bool x =
  theConstructor x

theConstructor_Bool :: Bool -> String
theConstructor_Bool False = "False"
theConstructor_Bool True  = "True"

inspect $ 'gtheConstructor_Bool === 'theConstructor_Bool

gtheConstructor_Ordering :: Ordering -> String
gtheConstructor_Ordering x =
  theConstructor x

theConstructor_Ordering :: Ordering -> String
theConstructor_Ordering LT = "LT"
theConstructor_Ordering EQ = "EQ"
theConstructor_Ordering GT = "GT"

inspect $ 'gtheConstructor_Ordering === 'theConstructor_Ordering

gtheConstructor_Maybe :: Maybe a -> String
gtheConstructor_Maybe x =
  theConstructor x

theConstructor_Maybe :: Maybe a -> String
theConstructor_Maybe Nothing  = "Nothing"
theConstructor_Maybe (Just _) = "Just"

inspect $ 'gtheConstructor_Maybe === 'theConstructor_Maybe

gtheConstructor_I10 :: I10 -> String
gtheConstructor_I10 x =
  theConstructor x

-- TODO: should this be strict?
theConstructor_I10 :: I10 -> String
theConstructor_I10 _ = "I10"

-- fails due to a strange combination of casts not being eliminated
inspect ('gtheConstructor_I10 === 'theConstructor_I10) { expectFail = True }

gtheConstructor_I10' :: I10' -> String
gtheConstructor_I10' x =
  theConstructor x

theConstructor_I10' :: I10' -> String
theConstructor_I10' !_ = "I10"

-- fails due to a strange combination of casts not being eliminated
inspect ('gtheConstructor_I10' === 'theConstructor_I10) { expectFail = True }

---------------------------------------------------------------------
-- injections

ginjections_NP0 :: NP (Injection f '[]) '[]
ginjections_NP0 = injections

injections_NP0 :: NP (Injection f '[]) '[]
injections_NP0 = Nil

inspect $ 'ginjections_NP0 === 'injections_NP0

ginjections_NP1 :: NP (Injection f '[a]) '[a]
ginjections_NP1 = injections

injections_NP1 :: NP (Injection f '[a]) '[a]
injections_NP1 =
  fn (\ x -> K (Z x)) :* Nil

inspect $ 'ginjections_NP1 === 'injections_NP1

ginjections_NP2 :: NP (Injection f '[a, b]) '[a, b]
ginjections_NP2 = injections

injections_NP2 :: NP (Injection f '[a, b]) '[a, b]
injections_NP2 =
  fn (\ x -> K (Z x)) :* fn (\ x -> K (S (Z x))) :* Nil

-- This one is quite sensitive to thresholds. It fails
-- for a higher unfolding-use-threshold.
inspect $ 'ginjections_NP2 === 'injections_NP2

---------------------------------------------------------------------
-- projections

gprojections_NP0 :: NP (Projection f '[]) '[]
gprojections_NP0 = projections

projections_NP0 :: NP (Projection f '[]) '[]
projections_NP0 = Nil

inspect $ 'gprojections_NP0 === 'projections_NP0

gprojections_NP1 :: NP (Projection f '[a]) '[a]
gprojections_NP1 = projections

projections_NP1 :: NP (Projection f '[a]) '[a]
projections_NP1 =
  fn (\ (K (x :* _)) -> x) :* Nil

inspect $ 'gprojections_NP1 === 'projections_NP1

gprojections_NP2 :: NP (Projection f '[a, b]) '[a, b]
gprojections_NP2 = projections

projections_NP2 :: NP (Projection f '[a, b]) '[a, b]
projections_NP2 =
  fn (\ (K (x :* _)) -> x) :* fn (\ (K (_ :* x :* _)) -> x) :* Nil

-- fails for unclear reasons
inspect ('gprojections_NP2 === 'projections_NP2) { expectFail = True }

---------------------------------------------------------------------
-- apInjs

genum :: IsEnumType a => [a]
genum =
  hcollapse genum'
{-# INLINE genum #-}

genum' :: IsEnumType a => NP (K a) (Code a)
genum' =
  hmap (mapKK to) (apInjs'_POP (POP (hcpure (Proxy :: Proxy ((~) '[])) Nil)))
{-# INLINE genum' #-}

-- Just for comparison. This is a non-idiomatic version of genum'
-- that has been manually transformed a little bit, just in case this
-- would change the required thresholds (but it doesn't seem to
-- be the case; at least not significantly).
genum'' :: IsEnumType a => NP (K a) (Code a)
genum'' =
  ap_NP (map_NP (fn . ((K . to . SOP . unK) .) . apFn) injections)
    (cpure_NP (Proxy :: Proxy ((~) '[])) Nil)
{-# INLINE genum'' #-}

genum'_Bool :: NP (K Bool) (Code Bool)
genum'_Bool = genum'

enum'_Bool :: NP (K Bool) (Code Bool)
enum'_Bool =
  K False :* K True :* Nil

inspect $ 'genum'_Bool === 'enum'_Bool

genum'_E1 :: NP (K E1) (Code E1)
genum'_E1 = genum'

enum'_E1 :: NP (K E1) (Code E1)
enum'_E1 =
  K E1_0 :* Nil

inspect $ 'genum'_E1 === 'enum'_E1

genum'_E1' :: NP (K E1') (Code E1')
genum'_E1' = genum'

enum'_E1' :: NP (K E1') (Code E1')
enum'_E1' =
  K E1'_0 :* Nil

inspect $ 'genum'_E1' === 'enum'_E1'

genum_E1 :: [E1]
genum_E1 = genum

enum_E1 :: [E1]
enum_E1 =
  [E1_0]

inspect $ 'genum_E1 === 'enum_E1

genum_E1' :: [E1']
genum_E1' = genum

enum_E1' :: [E1']
enum_E1' =
  [E1'_0]

inspect $ 'genum_E1' === 'enum_E1'

genum'_E2 :: NP (K E2) (Code E2)
genum'_E2 = genum'

enum'_E2 :: NP (K E2) (Code E2)
enum'_E2 =
  K E2_0 :* K E2_1 :* Nil

inspect $ 'genum'_E2 === 'enum'_E2

genum'_E2' :: NP (K E2') (Code E2')
genum'_E2' = genum'

enum'_E2' :: NP (K E2') (Code E2')
enum'_E2' =
  K E2'_0 :* K E2'_1 :* Nil

inspect $ 'genum'_E2' === 'enum'_E2'

genum'_E3 :: NP (K E3) (Code E3)
genum'_E3 = genum'

enum'_E3 :: NP (K E3) (Code E3)
enum'_E3 =
  K E3_0 :* K E3_1 :* K E3_2 :* Nil

inspect $ 'genum'_E3 === 'enum'_E3

genum'_E3' :: NP (K E3') (Code E3')
genum'_E3' = genum'

enum'_E3' :: NP (K E3') (Code E3')
enum'_E3' =
  K E3'_0 :* K E3'_1 :* K E3'_2 :* Nil

inspect $ 'genum'_E3' === 'enum'_E3'

genum_E3 :: [E3]
genum_E3 = genum

enum_E3 :: [E3]
enum_E3 =
  [E3_0, E3_1, E3_2]

inspect $ 'genum_E3 === 'enum_E3

genum_E3' :: [E3']
genum_E3' = genum

enum_E3' :: [E3']
enum_E3' =
  [E3'_0, E3'_1, E3'_2]

inspect $ 'genum_E3' === 'enum_E3'

genum'_E5 :: NP (K E5) (Code E5)
genum'_E5 = genum'

enum'_E5 :: NP (K E5) (Code E5)
enum'_E5 =
  K E5_0 :* K E5_1 :* K E5_2 :* K E5_3 :* K E5_4 :* Nil

inspect $ 'genum'_E5 === 'enum'_E5

genum'_E10 :: NP (K E10) (Code E10)
genum'_E10 = genum'

enum'_E10 :: NP (K E10) (Code E10)
enum'_E10 =
  K E10_0 :* K E10_1 :* K E10_2 :* K E10_3 :* K E10_4 :* K E10_5 :* K E10_6 :* K E10_7 :* K E10_8 :* K E10_9 :* Nil

-- needs higher threshold
inspect ('genum'_E10 === 'enum'_E10) { expectFail = True }

genum'_E10' :: NP (K E10') (Code E10')
genum'_E10' = genum'

enum'_E10' :: NP (K E10') (Code E10')
enum'_E10' =
  K E10'_0 :* K E10'_1 :* K E10'_2 :* K E10'_3 :* K E10'_4 :* K E10'_5 :* K E10'_6 :* K E10'_7 :* K E10'_8 :* K E10'_9 :* Nil

-- needs higher threshold
inspect ('genum'_E10' === 'enum'_E10') { expectFail = True }

main :: IO ()
main = return ()

