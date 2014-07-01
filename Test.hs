{-# LANGUAGE TemplateHaskell, DataKinds, TypeFamilies, KindSignatures, DeriveGeneric #-}

import Generics.SOP
import Generics.SOP.TH

import qualified GHC.Generics as GHC

-- import Data.Map
import Data.Ratio

data Tree (f :: * -> *) a =
  Leaf (f a) | Node (Tree f a) (Tree f a)
  deriving (GHC.Generic)

deriveGeneric ''Tree

deriveGeneric ''Maybe

deriveGeneric ''Either

deriveGeneric ''Ratio

-- instance HasDatatypeInfo (Ratio a)
