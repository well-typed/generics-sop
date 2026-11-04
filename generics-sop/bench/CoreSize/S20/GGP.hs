{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
-- | The "CoreSize.S20.TH" datatype, derived via "GHC.Generics" instead, as a
-- baseline to compare the Template Haskell representation against.
module CoreSize.S20.GGP where

import qualified Generics.SOP as SOP
import qualified GHC.Generics as GHC

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
  deriving (GHC.Generic)

instance SOP.Generic         S20
instance SOP.HasDatatypeInfo S20
