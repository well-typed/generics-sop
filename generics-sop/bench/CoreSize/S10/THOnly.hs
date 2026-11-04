{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
-- | As "CoreSize.S10.TH" but without the metadata instance, so that it is a
-- like-for-like baseline for "CoreSize.S10.Inj", which defines Generic only.
module CoreSize.S10.THOnly where

import Generics.SOP.TH

data S10 =
    S10_00
  | S10_01
  | S10_02
  | S10_03
  | S10_04
  | S10_05
  | S10_06
  | S10_07
  | S10_08
  | S10_09

deriveGenericOnly ''S10
