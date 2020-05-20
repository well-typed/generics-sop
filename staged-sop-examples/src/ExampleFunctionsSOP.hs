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
module ExampleFunctionsSOP where

import Codec.CBOR.Encoding
import Codec.CBOR.Decoding
import Codec.Serialise
import Control.DeepSeq
import Data.SOP.Constraint (And)
import Generics.SOP
import Generics.SOP.NP
import Generics.SOP.NS
import ExampleTypes

-- SOP utilities
--
type Description a = Code a

selectWith'_NS :: All Top xs => (forall x . f x -> g x -> r) -> NP f xs -> NS g xs -> r
selectWith'_NS op npf npg = collapse_NS (hzipWith (\ fx gx -> K (op fx gx)) npf npg)

-- Vanilla SOP generic functions

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
  collapse_NS (hzipWith const names (enumTypeFrom a))

s15Names :: NP (K String) (Description S15)
s15Names =
  K "1" :* K "2" :* K "3" :* K "4" :* K "5" :* K "6" :* K "7" :* K "8" :* K "9" :* K "10" :* K "11" :* K "12" :* K "13" :* K "14" :* K "15" :* Nil

geq ::
  (Generic a, All (All Eq) (Description a)) =>
  a -> a -> Bool
geq a1 a2 =
  ccompare_SOP (Proxy @Eq)
    False
    (\ xs1 xs2 -> and (collapse_NP (czipWith_NP (Proxy @Eq) (mapIIK (==)) xs1 xs2)))
    False
    (from a1) (from a2)

genum :: (Generic a, All ((~) '[]) (Description a)) => [a]
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
