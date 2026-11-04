{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
-- | The "CoreSize.S10.TH" datatype, derived via "GHC.Generics" instead, as a
-- baseline to compare the Template Haskell representation against.
module CoreSize.S10.GGP where

import qualified Generics.SOP as SOP
import qualified GHC.Generics as GHC

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
  deriving (GHC.Generic)

instance SOP.Generic         S10
instance SOP.HasDatatypeInfo S10
