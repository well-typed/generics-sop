{-# LANGUAGE TemplateHaskell, DataKinds, TypeFamilies, KindSignatures #-}

import Generics.SOP
import Generics.SOP.TH

import Data.Map

data Tree (f :: * -> *) a =
  Leaf (f a) | Node (Tree f a) (Tree f a)
deriveGeneric ''Tree

deriveGeneric ''Maybe

deriveGeneric ''Either

