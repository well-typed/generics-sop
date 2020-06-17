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
{-# OPTIONS_GHC -Wall #-}
module ExampleFunctionsSOP where

import Codec.CBOR.Encoding
import Codec.CBOR.Decoding
import Codec.Serialise
import Generics.SOP
import Generics.SOP.NP
import Generics.SOP.NS
import Text.Show.Pretty

-- SOP utilities
--
type Description a = Code a

selectWith'_NS :: All Top xs => (forall x . f x -> g x -> r) -> NP f xs -> NS g xs -> r
selectWith'_NS op npf npg = collapse_NS (hzipWith (\ fx gx -> K (op fx gx)) npf npg)

cselectWith'_NS :: All c xs => proxy c -> (forall x . c x => f x -> g x -> r) -> NP f xs -> NS g xs -> r
cselectWith'_NS p op npf npg = collapse_NS (hczipWith p (\ fx gx -> K (op fx gx)) npf npg)

-- Vanilla SOP generic functions

gmempty ::
  (IsProductType a xs, All Monoid xs) => a
gmempty =
  productTypeTo
    (cpure_NP (Proxy @Monoid) (I mempty))

gsappend ::
  (IsProductType a xs, All Semigroup xs) =>
  a -> a -> a
gsappend a1 a2 =
  productTypeTo
    (czipWith_NP (Proxy @Semigroup)
      (mapIII (<>)) (productTypeFrom a1) (productTypeFrom a2)
    )

gShowEnum ::
  IsEnumType a => NP (K String) (Description a) -> a -> String
gShowEnum names a =
  selectWith'_NS ((unK .) . const) names (enumTypeFrom a)

gPrettyVal ::
  forall a . (Generic a, HasDatatypeInfo a, All (All PrettyVal) (Description a)) =>
  a -> Value
gPrettyVal a =
  selectWith'_NS go
    (constructorInfo (datatypeInfo (Proxy @a)))
    (unSOP (cmap_SOP (Proxy @PrettyVal) (mapIK prettyVal) (from a) :: SOP (K Value) (Description a)))
  where
    go :: forall xs . ConstructorInfo xs -> NP (K Value) xs -> Value
    go (Constructor n) np = Con n (collapse_NP np)
    go (Infix n _ _) np   = Con n (collapse_NP np)
    go (Record n fs) np   = Rec n (collapse_NP (zipWith_NP (\ (FieldInfo f) (K x) -> K (f, x)) fs np))

geq ::
  (Generic a, All (All Eq) (Description a)) =>
  a -> a -> Bool
geq a1 a2 =
  ccompare_SOP (Proxy @Eq)
    False
    (\ xs1 xs2 -> and (collapse_NP (czipWith_NP (Proxy @Eq) (mapIIK (==)) xs1 xs2)))
    False
    (from a1) (from a2)

genum :: IsEnumType a => [a]
genum =
  to <$> apInjs_POP (POP (cpure_NP (Proxy @((~) '[])) Nil))

conNumbers :: Generic a => Proxy a -> NP (K Word) (Description a)
conNumbers _ =
  ana_NP (\ (K i) -> (K i, K (i + 1))) (K 0)

conArities :: Generic a => Proxy a -> NP (K Word) (Description a)
conArities _ =
  let
    go :: forall xs . SListI xs => K Word xs
    go = K (fromIntegral (lengthSList (Proxy @xs)))
  in
    cpure_NP (Proxy @SListI) go

conTable :: forall a . Generic a => Proxy a -> NP (K (Word, Word)) (Description a)
conTable p =
  zipWith_NP
    (mapKKK (,))
    (conNumbers p)
    (conArities p)

gencode :: forall a . (Generic a, All (All Serialise) (Description a)) => a -> Encoding
gencode x =
  let
    encodedConstructorArguments :: SOP (K Encoding) (Description a)
    encodedConstructorArguments =
      cmap_SOP (Proxy @Serialise) (mapIK encode) (from x)
  in
    selectWith'_NS
      (\ (K (i, a)) es ->
        encodeListLen (a + 1)
          <> encodeWord i
          <> mconcat (collapse_NP es)
      )
      (conTable (Proxy @a))
      (unSOP encodedConstructorArguments)

gdecode :: forall a s . (Generic a, All (All Serialise) (Description a)) => Decoder s a
gdecode =
  let
    decoderConstructorArguments :: NP (K (SOP (Decoder s) (Description a))) (Description a)
    decoderConstructorArguments =
      apInjs'_POP (cpure_POP (Proxy @Serialise) decode)

    decoderTable :: NP (K ((Word, Word), Decoder s a)) (Description a)
    decoderTable =
      zipWith_NP
        (\ (K (i, a)) (K dec) -> K ((a + 1, i), to <$> sequence_SOP dec))
        (conTable (Proxy @a))
        decoderConstructorArguments
  in
    do
      len <- fromIntegral <$> decodeListLen
      tag <- decodeWord
      case lookup (len, tag) (collapse_NP decoderTable) of
        Just dec -> dec
        Nothing  -> fail "invalid encoding"
