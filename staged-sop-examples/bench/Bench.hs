{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

{-# LANGUAGE IncoherentInstances #-}

{-# OPTIONS_GHC -ddump-splices #-}
-- {-# OPTIONS_GHC -ddump-simpl -dsuppress-all #-}
module Main where

import Codec.CBOR.Encoding
import Codec.CBOR.Decoding
import Codec.Serialise
import ExampleTypes
import ExampleFunctions
import Gauge.Main
import Gauge.Main.Options
import Generics.SOP.Staged

genum_Ordering :: () -> [Ordering]
genum_Ordering () = genum

sgenum_Ordering :: () -> [Ordering]
sgenum_Ordering () = $$(sgenum)

gShowEnum_S15 :: S15 -> String
gShowEnum_S15 = gShowEnum s15Names

sgShowEnum_S15 :: S15 -> String
sgShowEnum_S15 x = $$(sgShowEnum s15Names [|| x ||])

gsappend_Foo :: Foo -> Foo -> Foo
gsappend_Foo = gsappend

sgsappend_Foo :: Foo -> Foo -> Foo
sgsappend_Foo  =
  let
    c = constraints @Foo
  in
    $$(sgsappend' (pallC [|| c ||]))

msappend_Foo :: Foo -> Foo -> Foo
msappend_Foo (Foo is1 o1 txt1) (Foo is2 o2 txt2) =
  Foo (is1 <> is2) (o1 <> o2) (txt1 <> txt2)

ghcsappend_Foo :: Foo -> Foo -> Foo
ghcsappend_Foo = ghcsappend

deriving instance Eq a => Eq (Tree StockDeriving a)

instance Eq a => Eq (Tree GenericsSOP a) where
  (==) = geq

instance Eq a => Eq (Tree GHCGenerics a) where
  (==) = ghcgeq

instance Eq a => Eq (Tree StagedSOP a) where
  x1 == x2 =
    let
      c = constraints @(Tree StagedSOP a)
    in
      $$(sgeq (allC [|| c ||]) [|| x1 ||] [|| x2 ||])

instance Eq a => Eq (Tree Manual a) where
  Leaf a1 == Leaf a2 = a1 == a2
  Node l1 r1 == Node l2 r2 = l1 == l2 && r1 == r2
  _ == _ = False

deriving instance Eq (Prop StockDeriving)

instance Eq (Prop GenericsSOP) where
  (==) = geq

instance Eq (Prop GHCGenerics) where
  (==) = ghcgeq

instance Eq (Prop StagedSOP) where
  x1 == x2 =
    let
      c = constraints @(Prop StagedSOP)
    in
      $$(sgeq (allC [|| c ||]) [|| x1 ||] [|| x2 ||])

instance Eq (Prop Manual) where
  Var x1 == Var x2 = x1 == x2
  T == T = True
  F == F = True
  Not p1 == Not p2 = p1 == p2
  And p1 q1 == And p2 q2 = p1 == p2 && q1 == q2
  Or p1 q1 == Or p2 q2 = p1 == p2 && q1 == q2
  _ == _ = False

instance Serialise (Prop GenericsSOP) where
  encode = gencode
  decode = gdecode

-- NOTE: decode currently requires IncoherentInstances because the
-- way we are passing the Applicative instance for Decoder via an
-- explicit dictionary, it ends up being in scope twice!
--
instance Serialise (Prop StagedSOP) where
  encode =
    let
      c = constraints @(Prop StagedSOP)
    in
      $$(sgencode (allC [|| c ||]))
  decode =
    let
      c = constraints @(Prop StagedSOP)
    in
      $$(sgdecode (allC [|| c ||]))

instance Serialise (Prop GHCGenerics)

instance Serialise (Prop Manual) where
  encode (Var x) = encodeListLen 2 <> encodeWord 0 <> encode x
  encode T       = encodeListLen 1 <> encodeWord 1
  encode F       = encodeListLen 1 <> encodeWord 2
  encode (Not p) = encodeListLen 2 <> encodeWord 3 <> encode p
  encode (And p1 p2) = encodeListLen 3 <> encodeWord 4 <> encode p1 <> encode p2
  encode (Or p1 p2)  = encodeListLen 3 <> encodeWord 5 <> encode p1 <> encode p2

  decode = do
    len <- decodeListLen
    tag <- decodeWord
    case (len, tag) of
      (2, 0) -> Var <$> decode
      (1, 1) -> pure T
      (1, 2) -> pure F
      (2, 3) -> Not <$> decode
      (3, 4) -> And <$> decode <*> decode
      (3, 5) -> Or <$> decode <*> decode
      _ -> fail "invalid Prop encoding"

roundtrip :: Serialise a => a -> a
roundtrip = deserialise . serialise

main :: IO ()
main = do
  -- guarding correctness
  print $ serialise @(Prop GenericsSOP) huge_prop
  print $ serialise @(Prop GHCGenerics) huge_prop
  print $ serialise @(Prop StagedSOP)   huge_prop
  print $ serialise @(Prop GenericsSOP) huge_prop == serialise @(Prop StagedSOP) huge_prop
  print $ serialise @(Prop GHCGenerics) huge_prop == serialise @(Prop GenericsSOP) huge_prop
  print $ serialise @(Prop Manual)      huge_prop == serialise @(Prop GenericsSOP) huge_prop
  print $ roundtrip @(Prop GenericsSOP) huge_prop == huge_prop
  print $ roundtrip @(Prop GHCGenerics) huge_prop == huge_prop
  print $ roundtrip @(Prop StagedSOP)   huge_prop == huge_prop
  print $ roundtrip @(Prop Manual)      huge_prop == huge_prop
  defaultMain
    [ -- bgroup "enum/Ordering"
      -- [ bench "generics-sop" $ nf genum_Ordering ()
      -- , bench "staged-sop"   $ nf sgenum_Ordering ()
      -- ]
      bgroup "showEnum/S15"
      [ bench "generics-sop"   $ nf gShowEnum_S15 S15_10
      , bench "staged-sop"     $ nf sgShowEnum_S15 S15_10
      ]
    , bgroup "sappend/Foo"
      [ bench "generics-sop"   $ nf (gsappend_Foo foo) foo
      , bench "staged-sop"     $ nf (sgsappend_Foo foo) foo
      , bench "manual"         $ nf (msappend_Foo foo) foo
      , bench "ghc-generics"   $ nf (ghcsappend_Foo foo) foo
      ]
    , bgroup "eq/Tree"
      [ env (return tree_large) $ \ t -> bench "generics-sop"   $ nf ((==) @(Tree GenericsSOP Int) t) t
      , env (return tree_large) $ \ t -> bench "stock-deriving" $ nf ((==) @(Tree StockDeriving Int) t) t
      , env (return tree_large) $ \ t -> bench "ghc-generics"   $ nf ((==) @(Tree GHCGenerics Int) t) t
      , env (return tree_large) $ \ t -> bench "staged-sop"     $ nf ((==) @(Tree StagedSOP Int) t) t
      , env (return tree_large) $ \ t -> bench "manual"         $ nf ((==) @(Tree Manual Int) t) t
      ]
    , bgroup "eq/Prop"
      [ env (return huge_prop) $ \ p -> bench "generics-sop"   $ nf ((==) @(Prop GenericsSOP) p) p
      , env (return huge_prop) $ \ p -> bench "stock-deriving" $ nf ((==) @(Prop StockDeriving) p) p
      , env (return huge_prop) $ \ p -> bench "ghc-generics"   $ nf ((==) @(Prop GHCGenerics) p) p
      , env (return huge_prop) $ \ p -> bench "staged-sop"     $ nf ((==) @(Prop StagedSOP) p) p
      , env (return huge_prop) $ \ p -> bench "manual"         $ nf ((==) @(Prop Manual) p) p
      ]
    , bgroup "cborg-serialise/Prop"
      [ bench "generics-sop"   $ nf (serialise @(Prop GenericsSOP)) huge_prop
      , bench "staged-sop"     $ nf (serialise @(Prop StagedSOP)) huge_prop
      , bench "ghc-generics"   $ nf (serialise @(Prop GHCGenerics)) huge_prop
      , bench "manual"         $ nf (serialise @(Prop Manual)) huge_prop
      ]
    , bgroup "cborg-roundtrip/Prop"
      [ bench "generics-sop"   $ nf (roundtrip @(Prop GenericsSOP)) huge_prop
      , bench "staged-sop"     $ nf (roundtrip @(Prop StagedSOP)) huge_prop
      , bench "ghc-generics"   $ nf (roundtrip @(Prop GHCGenerics)) huge_prop
      , bench "manual"         $ nf (roundtrip @(Prop Manual)) huge_prop
      ]
    ]
