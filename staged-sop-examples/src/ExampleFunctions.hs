{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module ExampleFunctions where

import Codec.CBOR.Encoding
import Codec.CBOR.Decoding
import Codec.Serialise
import Control.DeepSeq
import Data.SOP.Constraint (And)
import qualified Generics.SOP as SOP
import qualified Generics.SOP.NP as SOP
import qualified Generics.SOP.NS as SOP
import Generics.SOP.Staged
import GHC.Generics as GHC hiding (C, (:.:))
import Language.Haskell.TH
import ExampleTypes

sgsappend ::
  (SIsProductType a xs, All (Quoted Semigroup) xs) =>
  Code a -> Code a -> Code a
sgsappend c1 c2 =
  sproductTypeFrom c1 $ \ a1 -> sproductTypeFrom c2 $ \ a2 ->
    sproductTypeTo
      (czipWith_NP (Proxy @(Quoted Semigroup))
        (mapCCC [|| (<>) ||]) a1 a2
      )

sgsappend' ::
  (SIsProductType a xs, All (Quoted Semigroup) xs) =>
  Code (a -> a -> a)
sgsappend' =
  [|| \ a1 a2 -> $$(sgsappend [|| a1 ||] [|| a2 ||]) ||]

gsappend ::
  (SOP.IsProductType a xs, SOP.All Semigroup xs) =>
  a -> a -> a
gsappend a1 a2 =
  SOP.productTypeTo
    (SOP.czipWith_NP (Proxy @Semigroup)
      (SOP.mapIII (<>)) (SOP.productTypeFrom a1) (SOP.productTypeFrom a2)
    )

class GSappend a where
  ghcsappend' :: a x -> a x -> a x

instance GSappend U1 where
  ghcsappend' U1 U1 = U1

instance (GSappend a, GSappend b) => GSappend (a :*: b) where
  ghcsappend' (a1 :*: b1) (a2 :*: b2) =
    ghcsappend' a1 a2 :*: ghcsappend' b1 b2

instance Semigroup a => GSappend (K1 r a) where
  ghcsappend' (K1 a1) (K1 a2) = K1 (a1 <> a2)

instance GSappend a => GSappend (M1 i c a) where
  ghcsappend' (M1 a1) (M1 a2) = M1 (ghcsappend' a1 a2)

ghcsappend :: (GHC.Generic a, GSappend (Rep a)) => a -> a -> a
ghcsappend a1 a2 = to (ghcsappend' (from a1) (from a2))


sgrnf ::
  (SGeneric a, All (All (Quoted NFData)) (SDescription a)) =>
  Code a -> Code ()
sgrnf c =
  sfrom c $ \ a ->
    foldr (\ x r -> [|| $$x `seq` $$r ||]) [|| () ||]
      (collapse_SOP (cmap_SOP (Proxy @(Quoted NFData)) (mapCK [|| rnf ||]) a))

gShowEnum ::
  SOP.IsEnumType a => NP (K String) (SOP.Code a) -> a -> String
gShowEnum names a =
  SOP.collapse_NS (SOP.hzipWith const names (SOP.enumTypeFrom a))

sgShowEnum ::
  SIsEnumType a => NP (K String) (SDescription a) -> Code a -> Code String
sgShowEnum names c =
  senumTypeFrom c $ \ a ->
    liftTyped (collapse_NS (selectWith_NS const names a))

s15Names :: NP (K String) (SOP.Code S15)
s15Names =
  K "1" :* K "2" :* K "3" :* K "4" :* K "5" :* K "6" :* K "7" :* K "8" :* K "9" :* K "10" :* K "11" :* K "12" :* K "13" :* K "14" :* K "15" :* Nil

geq ::
  (SOP.Generic a, SOP.All (SOP.All Eq) (SOP.Code a)) =>
  a -> a -> Bool
geq a1 a2 =
  SOP.ccompare_SOP (Proxy @Eq)
    False
    (\ xs1 xs2 -> and (SOP.collapse_NP (SOP.czipWith_NP (Proxy @Eq) (SOP.mapIIK (==)) xs1 xs2)))
    False
    (SOP.from a1) (SOP.from a2)

sgeq ::
  (SGeneric a, All (All (Quoted Eq)) (SDescription a)) =>
  Code a -> Code a -> Code Bool
sgeq c1 c2 =
  sfrom c1 $ \ a1 -> sfrom c2 $ \ a2 ->
  ccompare_SOP (Proxy @(Quoted Eq))
    [|| False ||]
    (\ xs1 xs2 -> sand (collapse_NP (czipWith_NP (Proxy @(Quoted Eq)) (mapCCK [|| (==) ||]) xs1 xs2)))
    [|| False ||]
    a1 a2

sand :: [Code Bool] -> Code Bool
sand = foldr (\ x r -> [|| $$x && $$r ||]) [|| True ||]

genum :: (SOP.Generic a, SOP.All ((~) '[]) (SOP.Code a)) => [a]
genum =
  SOP.to <$> SOP.apInjs_POP (POP (SOP.cpure_NP (Proxy @((~) '[])) Nil))

sgenum :: SIsEnumType a => Code [a]
sgenum =
  foldr (\ x r -> [|| $$x : $$r ||]) [|| [] ||]
    (sto <$> apInjs_POP (POP (cpure_NP (Proxy @((~) '[])) Nil)))

class GEq a where
  ghcgeq' :: a x -> a x -> Bool

instance GEq U1 where
  ghcgeq' U1 U1 = True

instance (GEq a, GEq b) => GEq (a :*: b) where
  ghcgeq' (a1 :*: b1) (a2 :*: b2) =
    ghcgeq' a1 a2 && ghcgeq' b1 b2

instance (GEq a, GEq b) => GEq (a :+: b) where
  ghcgeq' (L1 a1) (L1 a2) = ghcgeq' a1 a2
  ghcgeq' (R1 b1) (R1 b2) = ghcgeq' b1 b2
  ghcgeq' _ _ = False

instance Eq a => GEq (K1 r a) where
  ghcgeq' (K1 a1) (K1 a2) = a1 == a2

instance GEq a => GEq (M1 i c a) where
  ghcgeq' (M1 a1) (M1 a2) = ghcgeq' a1 a2

ghcgeq :: (GHC.Generic a, GEq (Rep a)) => a -> a -> Bool
ghcgeq a1 a2 = ghcgeq' (from a1) (from a2)

conNumbers :: forall a . (SOP.Generic a) => Proxy a -> NP (K Word) (SOP.Code a)
conNumbers _ =
  SOP.ana_NP (\ (K i) -> (K i, K (i + 1))) (K 0)

sconNumbers :: forall a . (SGeneric a) => Proxy a -> NP (K Word) (SDescription a)
sconNumbers _ =
  ana_NP (\ (K i) -> (K i, K (i + 1))) (K 0)

conArities :: forall a . (SOP.Generic a) => Proxy a -> NP (K Word) (SOP.Code a)
conArities _ =
  let
    go :: forall xs . SOP.SListI xs => K Word xs
    go = K (fromIntegral (SOP.lengthSList (Proxy @xs)))
  in
    SOP.cpure_NP (Proxy @SOP.SListI) go

sconArities :: forall a . (SGeneric a) => Proxy a -> NP (K Word) (SDescription a)
sconArities _ =
  let
    go :: forall xs . SListI xs => K Word xs
    go = K (fromIntegral (lengthSList (Proxy @xs)))
  in
    cpure_NP (Proxy @SListI) go


gencode :: forall a . (SOP.Generic a, SOP.All (SOP.All Serialise) (SOP.Code a)) => a -> Encoding
gencode x =
  let
    tmp :: SOP (K Encoding) (SOP.Code a)
    tmp =
      SOP.cmap_SOP (Proxy @Serialise) (SOP.mapIK encode) (SOP.from x)

    tmp2 :: NS (K Encoding) (SOP.Code a)
    tmp2 =
      SOP.hzipWith3
        (\ (K i) (K a) es -> K (encodeListLen (a + 1) <> encodeWord i <> mconcat (SOP.collapse_NP es)))
        (conNumbers (Proxy @a))
        (conArities (Proxy @a))
        (SOP.unSOP tmp)
  in
    SOP.collapse_NS tmp2

sgencode :: forall a . (SGeneric a, All (All (Quoted Serialise)) (SDescription a)) => Code (a -> Encoding)
sgencode =
  [||
    \ a ->
    $$(sfrom [|| a ||] $ \ a' ->
      let
        tmp :: SOP (K (Code Encoding)) (SDescription a)
        tmp =
          cmap_SOP (Proxy @(Quoted Serialise)) (mapCK [|| encode ||]) a'

        tmp2 :: NS (K (Code Encoding)) (SDescription a)
        tmp2 =
          cselectWith_NS
            (Proxy @(All (Quoted Serialise)))
            (\ (K (i, a)) es -> let a' = a + 1 in K [|| encodeListLen a' <> encodeWord i <> $$(foldr (\ x y -> [|| $$x <> $$y ||]) [|| mempty ||] (collapse_NP es)) ||])
            (tmp3 @a) -- (czipWith_NP (Proxy @(All (Quoted Serialise))) (SOP.mapKKK (,)) (sconNumbers (Proxy @a)) (sconArities (Proxy @a)))
            (unSOP tmp)

        tmp3 :: forall a . (SGeneric a, All (All (Quoted Serialise)) (SDescription a)) => NP (K (Word, Word)) (SDescription a)
        tmp3 =
          (czipWith_NP (Proxy @(All (Quoted Serialise))) (SOP.mapKKK (,)) (sconNumbers (Proxy @a)) (sconArities (Proxy @a)))
      in
        collapse_NS tmp2
    )
  ||]

gdecode :: forall a s . (SOP.Generic a, SOP.All (SOP.All Serialise) (SOP.Code a)) => Decoder s a
gdecode =
  let
    tmp :: POP (Decoder s) (SOP.Code a)
    tmp =
      SOP.cpure_POP (Proxy @Serialise) decode

    tmp2 :: NP (K (SOP (Decoder s) (SOP.Code a))) (SOP.Code a)
    tmp2 =
      SOP.apInjs'_POP tmp

    tmp3 :: NP (K ((Word, Word), Decoder s a)) (SOP.Code a)
    tmp3 =
      SOP.zipWith3_NP
        (\ (K i) (K a) (K dec) -> K ((a + 1, i), SOP.to <$> SOP.sequence_SOP dec))
        (conNumbers (Proxy @a))
        (conArities (Proxy @a))
        tmp2
  in
    do
      len <- fromIntegral <$> decodeListLen
      tag <- decodeWord
      case lookup (len, tag) (collapse_NP tmp3) of
        Just dec -> dec
        Nothing  -> fail "invalid encoding"

sgdecode :: forall a s . (SGeneric a, LiftT s, All (All (Quoted Serialise)) (SDescription a), All (And (All LiftT) (AllTails (LiftTCurry a))) (SDescription a)) => Code (Decoder s a)
sgdecode =
  let
    tmp :: POP (C :.: Decoder s) (SDescription a)
    tmp =
      cpure_POP (Proxy @(Quoted Serialise)) (Comp (C [|| decode ||]))

    tmp2 :: NP (K (SOP (C :.: Decoder s) (SDescription a))) (SDescription a)
    tmp2 =
      apInjs'_POP tmp

    tmp3 :: NP (K ((Word, Word), Code (Decoder s a))) (SDescription a)
    tmp3 =
      czipWith_NP
        (Proxy @(All (Quoted Serialise)))
        (\ (K (i, a)) (K dec) -> K ((a + 1, i), stoA dec))
        (tmp3' @a)
        tmp2

    tmp3' :: forall a . (SGeneric a, All (All (Quoted Serialise)) (SDescription a)) => NP (K (Word, Word)) (SDescription a)
    tmp3' =
      (czipWith_NP (Proxy @(All (Quoted Serialise))) (SOP.mapKKK (,)) (sconNumbers (Proxy @a)) (sconArities (Proxy @a)))

    tmp4 :: [((Word, Word), Code (Decoder s a))]
    tmp4 =
      collapse_NP tmp3

    tmp5 :: Code (Word, Word) -> [((Word, Word), Code (Decoder s a))] -> Code (Decoder s a)
    tmp5 _sym []                  = [|| fail "invalid encoding" ||]
    tmp5 sym ((key, rhs) : cases) =
      [|| if $$sym == key then $$rhs else $$(tmp5 sym cases) ||]
  in
    [||
      do
        len <- fromIntegral <$> decodeListLen
        tag <- decodeWord
        $$(tmp5 [|| (len, tag) ||] tmp4)
    ||]
