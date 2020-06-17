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
module ExampleFunctionsStaged where

import Codec.CBOR.Encoding
import Codec.CBOR.Decoding
import Codec.Serialise
import Generics.SOP.Staged

gmempty ::
  (IsProductType a xs, All (Quoted Monoid) xs) => Code a
gmempty =
  productTypeTo
    (cpure_NP (Proxy @(Quoted Monoid)) (C [|| mempty ||]))

gsappend ::
  (IsProductType a xs, All (Quoted Semigroup) xs) =>
  Code a -> Code a -> Code a
gsappend c1 c2 =
  productTypeFrom c1 $ \ a1 -> productTypeFrom c2 $ \ a2 ->
  productTypeTo
    (czipWith_NP (Proxy @(Quoted Semigroup))
      (mapCCC [|| (<>) ||]) a1 a2
    )

gShowEnum ::
  IsEnumType a => NP (K String) (Description a) ->
  Code a -> Code String
gShowEnum names c =
  enumTypeFrom c $ \ a ->
  selectWith'_NS (((liftTyped . unK) .) . const) names a

geq ::
  (Generic a, All (All (Quoted Eq)) (Description a)) =>
  Code a -> Code a -> Code Bool
geq c1 c2 =
  from c1 $ \ a1 -> from c2 $ \ a2 ->
  ccompare_SOP (Proxy @(Quoted Eq))
    [|| False ||]
    (\ xs1 xs2 -> sand (collapse_NP (czipWith_NP (Proxy @(Quoted Eq)) (mapCCK [|| (==) ||]) xs1 xs2)))
    [|| False ||]
    a1 a2

sand :: [Code Bool] -> Code Bool
sand = foldr (\ x y -> [|| $$x && $$y ||]) [|| True ||]

genum :: IsEnumType a => Code [a]
genum =
  slist (to <$> apInjs_POP (POP (cpure_NP (Proxy @((~) '[])) Nil)))

slist :: LiftT a => [Code a] -> Code [a]
slist = foldr (\ x y -> [|| $$x : $$y ||]) [|| [] ||]

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

gencode ::
  forall a . (Generic a, All (All (Quoted Serialise)) (Description a)) =>
  Code a -> Code Encoding
gencode c = from c $ \ x ->
  let
    encodedConstructorArguments :: SOP (K (Code Encoding)) (Description a)
    encodedConstructorArguments =
      cmap_SOP (Proxy @(Quoted Serialise)) (mapCK [|| encode ||]) x
  in
    selectWith'_NS
      (\ (K (i, a)) es ->
        [|| encodeListLen $$(liftTyped (a + 1))
              <> encodeWord $$(liftTyped i)
              <> $$(smconcat (collapse_NP es))
        ||]
      )
      (conTable (Proxy @a))
      (unSOP encodedConstructorArguments)

smconcat :: [Code Encoding] -> Code Encoding
smconcat =
  foldr (\ x y -> [|| $$x <> $$y ||]) [|| mempty ||]

gdecode ::
  forall a s .
  (Generic a, All (All (Quoted Serialise)) (Description a), LiftT s, All (All LiftT `And` AllTails (LiftTCurry a)) (Description a)) => Code (Decoder s a)
gdecode =
  let
    decoderConstructorArguments :: NP (K (SOP (C :.: Decoder s) (Description a))) (Description a)
    decoderConstructorArguments =
      apInjs'_POP (cpure_POP (Proxy @(Quoted Serialise)) (Comp (C [|| decode ||])))

    decoderTable :: NP (K ((Word, Word), Code (Decoder s a))) (Description a)
    decoderTable =
      zipWith_NP
        (\ (K (i, a)) (K dec) -> K ((a + 1, i), toA dec))
        (conTable (Proxy @a))
        decoderConstructorArguments
  in
    [||
       do
         len <- fromIntegral <$> decodeListLen
         tag <- decodeWord
         $$(slookup [|| (len, tag) ||] (collapse_NP decoderTable) [|| fail "invalid encoding" ||])
    ||]

slookup :: (LiftT val, Quoted Eq key, Lift key) => Code key -> [(key, Code val)] -> Code val -> Code val
slookup _ [] fk = fk
slookup c ((k, v) : rest) fk =
  [|| if $$c == $$(liftTyped k) then $$v else $$(slookup c rest fk) ||]
