{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
-- | As "CoreSize.S40.TH" but without the metadata instance, so that it is a
-- like-for-like baseline for "CoreSize.S40.Inj", which defines Generic only.
module CoreSize.S40.THOnly where

import Generics.SOP.TH

data S40 =
    S40_00
  | S40_01
  | S40_02
  | S40_03
  | S40_04
  | S40_05
  | S40_06
  | S40_07
  | S40_08
  | S40_09
  | S40_10
  | S40_11
  | S40_12
  | S40_13
  | S40_14
  | S40_15
  | S40_16
  | S40_17
  | S40_18
  | S40_19
  | S40_20
  | S40_21
  | S40_22
  | S40_23
  | S40_24
  | S40_25
  | S40_26
  | S40_27
  | S40_28
  | S40_29
  | S40_30
  | S40_31
  | S40_32
  | S40_33
  | S40_34
  | S40_35
  | S40_36
  | S40_37
  | S40_38
  | S40_39

deriveGenericOnly ''S40
