{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
-- | Twice the constructors of "CoreSize.S10.TH", to show how the Core size of
-- the generated representation scales.
module CoreSize.S20.TH where

import Generics.SOP.TH

data S20 =
    S20_00
  | S20_01
  | S20_02
  | S20_03
  | S20_04
  | S20_05
  | S20_06
  | S20_07
  | S20_08
  | S20_09
  | S20_10
  | S20_11
  | S20_12
  | S20_13
  | S20_14
  | S20_15
  | S20_16
  | S20_17
  | S20_18
  | S20_19

deriveGeneric ''S20
