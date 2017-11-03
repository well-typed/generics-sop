{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Proofs.Types where

import Generics.SOP
import Generics.SOP.TH
import qualified GHC.Generics as GHC

data T1 a = T1 a
  deriving (GHC.Generic, Show)

instance Generic (T1 a)
instance HasDatatypeInfo (T1 a)

data T1' a = T1' a
  deriving (Show)

deriveGeneric ''T1'

data T2 a b = T2 a b
  deriving (GHC.Generic, Show)

instance Generic (T2 a b)
instance HasDatatypeInfo (T2 a b)

data T2' a b = T2' a b
  deriving (Show)

deriveGeneric ''T2'

idT2 :: T2 a b -> T2 a b
idT2 = \ x -> x
{-# INLINE idT2 #-}

idT2' :: T2' a b -> T2' a b
idT2' = \ x -> x
{-# INLINE idT2' #-}

data T3 a b c = T3 a b c
  deriving (GHC.Generic, Show)

instance Generic (T3 a b c)
instance HasDatatypeInfo (T3 a b c)

data T3' a b c = T3' a b c
  deriving (GHC.Generic, Show)

deriveGeneric ''T3'

data I10 = I10
  Int
  Int
  Int
  Int
  Int
  Int
  Int
  Int
  Int
  Int
  deriving (GHC.Generic, Show)

instance Generic I10
instance HasDatatypeInfo I10

data I10' = I10'
  Int
  Int
  Int
  Int
  Int
  Int
  Int
  Int
  Int
  Int
  deriving (Show)

deriveGeneric ''I10'

productFrom :: IsProductType a xs => a -> NP I xs
productFrom = unZ . unSOP . from
{-# INLINE productFrom #-}

productTo :: IsProductType a xs => NP I xs -> a
productTo = to . SOP . Z
{-# INLINE productTo #-}

data Wrap1 c t = Wrap1 (forall a1 . (c a1) => t a1)
data Wrap2 c t = Wrap2 (forall a1 a2 . (c a1, c a2) => t a1 a2)
data Wrap3 c t = Wrap3 (forall a1 a2 a3 . (c a1, c a2, c a3) => t a1 a2 a3)

data Wrap1' c t r = Wrap1' (forall a1 . (c a1) => t a1 -> r)
data Wrap2' c t r = Wrap2' (forall a1 a2 . (c a1, c a2) => t a1 a2 -> r)
data Wrap3' c t r = Wrap3' (forall a1 a2 a3 . (c a1, c a2, c a3) => t a1 a2 a3 -> r)

data Wrap1'' c t = Wrap1'' (forall a1 . (c a1) => t a1 -> t a1 -> t a1)
data Wrap2'' c t = Wrap2'' (forall a1 a2 . (c a1, c a2) => t a1 a2 -> t a1 a2 -> t a1 a2)
data Wrap3'' c t = Wrap3'' (forall a1 a2 a3 . (c a1, c a2, c a3) => t a1 a2 a3 -> t a1 a2 a3 -> t a1 a2 a3)
