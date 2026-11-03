{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
-- | One datatype, one derivation mode, nothing else, so that the Core size
-- reported for this module is attributable to @deriveGeneric@ alone.
module CoreSize.S10.TH where

import Generics.SOP.TH

data S10 =
    S10_0
  | S10_1
  | S10_2
  | S10_3
  | S10_4
  | S10_5
  | S10_6
  | S10_7
  | S10_8
  | S10_9

deriveGeneric ''S10
