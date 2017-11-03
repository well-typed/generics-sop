{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fshow-hole-constraints -Wall #-}
{-# OPTIONS_GHC -O -fplugin GHC.Proof.Plugin #-}
module Main where

import Data.Monoid (Sum(..), Product(..), (<>))
import Generics.SOP
import GHC.Proof

import Proofs.Metadata
import Proofs.Types

---------------------------------------------------------------------
-- Simple properties

proof_caseSelf_T2 :: Proof
proof_caseSelf_T2 =
  (\ x -> case x of T2 a b -> T2 a b)
  ===
  (\ x -> x)

proof_caseSelf_Nil :: Proof
proof_caseSelf_Nil =
  (\ x -> case (x :: NP I '[]) of Nil -> Nil)
  ===
  (\ x -> x)

{-
-- fails
proof_caseSelf_ConsNil :: Proof
proof_caseSelf_ConsNil =
  (\ x -> case (x :: NP I '[Int]) of y :* Nil -> y :* Nil)
  ===
  (\ x -> x)
-}

---------------------------------------------------------------------
-- Roundtrips

proof_roundtrip_T2 :: Proof
proof_roundtrip_T2 =
  to . from
  ===
  idT2

proof_roundtrip_T2' :: Proof
proof_roundtrip_T2' =
  to . from
  ===
  idT2'

proof_doubleRoundtrip_T2 :: Proof
proof_doubleRoundtrip_T2 =
  roundt . roundt
  ===
  idT2
  where
    roundt :: T2 a b -> T2 a b
    roundt = to . from
    {-# INLINE roundt #-}

proof_doubleRoundtrip_T2' :: Proof
proof_doubleRoundtrip_T2' =
  roundt . roundt
  ===
  idT2'
  where
    roundt :: T2' a b -> T2' a b
    roundt = to . from
    {-# INLINE roundt #-}

proof_productRoundtrip_T2 :: Proof
proof_productRoundtrip_T2 =
  productTo . productFrom'
  ===
  idT2
  where
    productFrom' :: T2 a b -> NP I '[a, b]
    productFrom' = productFrom
    {-# INLINE productFrom' #-}

proof_productRoundtrip_T2' :: Proof
proof_productRoundtrip_T2' =
  productTo . productFrom'
  ===
  idT2'
  where
    productFrom' :: T2' a b -> NP I '[a, b]
    productFrom' = productFrom
    {-# INLINE productFrom' #-}

---------------------------------------------------------------------
-- cpure

gmempty :: (IsProductType a xs, All Monoid xs) => a
gmempty =
  productTo (hcpure (Proxy :: Proxy Monoid) (I mempty))
{-# INLINE gmempty #-}

mempty_T2 :: (Monoid a, Monoid b) => T2 a b
mempty_T2 =
  T2 mempty mempty

mempty_T2' :: (Monoid a, Monoid b) => T2' a b
mempty_T2' =
  T2' mempty mempty

mempty_T3 :: (Monoid a, Monoid b, Monoid c) => T3 a b c
mempty_T3 =
  T3 mempty mempty mempty

mempty_T3' :: (Monoid a, Monoid b, Monoid c) => T3' a b c
mempty_T3' =
  T3' mempty mempty mempty

proof_mempty_T2 :: Proof
proof_mempty_T2 =
  (Wrap2 gmempty :: Wrap2 Monoid T2)
  ===
  Wrap2 mempty_T2

proof_mempty_T2' :: Proof
proof_mempty_T2' =
  (Wrap2 gmempty :: Wrap2 Monoid T2')
  ===
  Wrap2 mempty_T2'

proof_mempty_T3 :: Proof
proof_mempty_T3 =
  (Wrap3 gmempty :: Wrap3 Monoid T3)
  ===
  Wrap3 mempty_T3

proof_mempty_T3' :: Proof
proof_mempty_T3' =
  (Wrap3 gmempty :: Wrap3 Monoid T3')
  ===
  Wrap3 mempty_T3'

proof_concreteMempty_Triple :: Proof
proof_concreteMempty_Triple =
  gmempty
  ===
  ((Sum 0, Product 1, []) :: (Sum Int, Product Int, [Bool]))

proof_concreteMempty_T3 :: Proof
proof_concreteMempty_T3 =
  gmempty
  ===
  (T3 (Sum 0) (Product 1) [] :: T3 (Sum Int) (Product Int) [Bool])

proof_concreteMempty_T3' :: Proof
proof_concreteMempty_T3' =
  gmempty
  ===
  (T3' (Sum 0) (Product 1) [] :: T3' (Sum Int) (Product Int) [Bool])

---------------------------------------------------------------------
-- cmap

gshow :: (Generic a, All2 Show (Code a)) => a -> String
gshow =
  concat . hcollapse . hcmap (Proxy :: Proxy Show) (mapIK show) . from
{-# INLINE gshow #-}

gproductShow :: (IsProductType a xs, All Show xs) => a -> String
gproductShow =
  concat . hcollapse . hcmap (Proxy :: Proxy Show) (mapIK show) . productFrom
{-# INLINE gproductShow #-}

{-
-- fails, due to GGP-conversion for single-constructor single-value datatype being lazy
proof_show_T1 :: Proof
proof_show_T1 =
  Wrap1' gshow
  ===
  (Wrap1' (\ (T1 x) -> show x) :: Wrap1' Show T1 String)
-}

proof_show_T1' :: Proof
proof_show_T1' =
  Wrap1' gshow
  ===
  (Wrap1' (\ (T1' x) -> show x) :: Wrap1' Show T1' String)

{-
-- fails, due to GGP-conversion for single-constructor single-value datatype being lazy
proof_productShow_T1 :: Proof
proof_productShow_T1 =
  Wrap1' gproductShow
  ===
  (Wrap1' (\ (T1 x) -> show x) :: Wrap1' Show T1 String)
-}

proof_productShow_T1' :: Proof
proof_productShow_T1' =
  Wrap1' gproductShow
  ===
  (Wrap1' (\ (T1' x) -> show x) :: Wrap1' Show T1' String)

proof_show_T2 :: Proof
proof_show_T2 =
  Wrap2' gshow
  ===
  (Wrap2' (\ (T2 x y) -> show x ++ show y) :: Wrap2' Show T2 String)

proof_show_T2' :: Proof
proof_show_T2' =
  Wrap2' gshow
  ===
  (Wrap2' (\ (T2' x y) -> show x ++ show y) :: Wrap2' Show T2' String)

proof_productShow_T2 :: Proof
proof_productShow_T2 =
  Wrap2' gproductShow
  ===
  (Wrap2' (\ (T2 x y) -> show x ++ show y) :: Wrap2' Show T2 String)

proof_productShow_T2' :: Proof
proof_productShow_T2' =
  Wrap2' gproductShow
  ===
  (Wrap2' (\ (T2' x y) -> show x ++ show y) :: Wrap2' Show T2' String)

proof_productShow_T3 :: Proof
proof_productShow_T3 =
  Wrap3' gproductShow
  ===
  (Wrap3' (\ (T3 x y z) -> show x ++ show y ++ show z) :: Wrap3' Show T3 String)

proof_productShow_T3' :: Proof
proof_productShow_T3' =
  Wrap3' gproductShow
  ===
  (Wrap3' (\ (T3' x y z) -> show x ++ show y ++ show z) :: Wrap3' Show T3' String)

---------------------------------------------------------------------
-- czipWith

gmappend :: (IsProductType a xs, All Monoid xs) => a -> a -> a
gmappend =
  \ x y -> productTo (hczipWith (Proxy :: Proxy Monoid) (mapIII mappend)
    (productFrom x) (productFrom y))
{-# INLINE gmappend #-}

{-
-- fails, due to GGP-conversion for single-constructor single-value datatype being lazy
proof_mappend_T1 :: Proof
proof_mappend_T1 =
  Wrap1'' gmappend
  ===
  (Wrap1'' (\ (T1 x1) (T1 x2) -> T1 (x1 <> x2)) :: Wrap1'' Monoid T1)
-}

proof_mappend_T1' :: Proof
proof_mappend_T1' =
  Wrap1'' gmappend
  ===
  (Wrap1'' (\ (T1' x1) (T1' x2) -> T1' (x1 <> x2)) :: Wrap1'' Monoid T1')

proof_mappend_T2 :: Proof
proof_mappend_T2 =
  Wrap2'' gmappend
  ===
  (Wrap2'' (\ (T2 x1 y1) (T2 x2 y2) -> T2 (x1 <> x2) (y1 <> y2)) :: Wrap2'' Monoid T2)

proof_mappend_T2' :: Proof
proof_mappend_T2' =
  Wrap2'' gmappend
  ===
  (Wrap2'' (\ (T2' x1 y1) (T2' x2 y2) -> T2' (x1 <> x2) (y1 <> y2)) :: Wrap2'' Monoid T2')

proof_mappend_T3 :: Proof
proof_mappend_T3 =
  Wrap3'' gmappend
  ===
  (Wrap3'' (\ (T3 x1 y1 z1) (T3 x2 y2 z2) -> T3 (x1 <> x2) (y1 <> y2) (z1 <> z2)) :: Wrap3'' Monoid T3)

proof_mappend_T3' :: Proof
proof_mappend_T3' =
  Wrap3'' gmappend
  ===
  (Wrap3'' (\ (T3' x1 y1 z1) (T3' x2 y2 z2) -> T3' (x1 <> x2) (y1 <> y2) (z1 <> z2)) :: Wrap3'' Monoid T3')

proof_concreteMappend_Triple :: Proof
proof_concreteMappend_Triple =
  gmappend
  ===
  (\ (Sum x1, Product x2, x3) (Sum y1, Product y2, y3) ->
    (Sum (x1 + y1 :: Int), Product (x2 * y2 :: Int), x3 ++ y3))

proof_concreteMappend_T3 :: Proof
proof_concreteMappend_T3 =
  gmappend
  ===
  (\ (T3 (Sum x1) (Product x2) x3) (T3 (Sum y1) (Product y2) y3) ->
    T3 (Sum (x1 + y1 :: Int)) (Product (x2 * y2 :: Int)) (x3 ++ y3))

---------------------------------------------------------------------
-- Metadata

proof_datatypeNameOf_T1 :: Proof
proof_datatypeNameOf_T1 =
  datatypeNameOf (Proxy :: Proxy (T1 ()))
  ===
  "T1"

proof_datatypeNameOf_T1' :: Proof
proof_datatypeNameOf_T1' =
  datatypeNameOf (Proxy :: Proxy (T1' ()))
  ===
  "T1'"

proof_datatypeNameOf_I10 :: Proof
proof_datatypeNameOf_I10 =
  datatypeNameOf (Proxy :: Proxy I10)
  ===
  "I10"

proof_datatypeNameOf_I10' :: Proof
proof_datatypeNameOf_I10' =
  datatypeNameOf (Proxy :: Proxy I10')
  ===
  "I10'"

proof_constructorNames_Bool :: Proof
proof_constructorNames_Bool =
  constructorNames (Proxy :: Proxy Bool)
  ===
  ["False", "True"]

proof_constructorNames_Ordering :: Proof
proof_constructorNames_Ordering =
  constructorNames (Proxy :: Proxy Ordering)
  ===
  ["LT", "EQ", "GT"]

proof_constructorNames_Maybe :: Proof
proof_constructorNames_Maybe =
  constructorNames (Proxy :: Proxy (Maybe Int))
  ===
  ["Nothing", "Just"]

proof_constructorNames_I10 :: Proof
proof_constructorNames_I10 =
  constructorNames (Proxy :: Proxy I10)
  ===
  ["I10"]

proof_constructorNames_I10' :: Proof
proof_constructorNames_I10' =
  constructorNames (Proxy :: Proxy I10')
  ===
  ["I10'"]

proof_theConstructor_True :: Proof
proof_theConstructor_True =
  theConstructor True
  ===
  "True"

proof_theConstructor_Bool :: Proof
proof_theConstructor_Bool =
  (\ x -> theConstructor x)
  ===
  (\ x -> case x of False -> "False"; True -> "True")

proof_theConstructor_Ordering :: Proof
proof_theConstructor_Ordering =
  (\ x -> theConstructor x)
  ===
  (\ x -> case x of LT -> "LT"; EQ -> "EQ"; GT -> "GT")

proof_theConstructor_Maybe :: Proof
proof_theConstructor_Maybe =
  (\ x -> theConstructor x)
  ===
  (\ x -> case x of Nothing -> "Nothing"; Just _ -> "Just")

main :: IO ()
main = return ()

