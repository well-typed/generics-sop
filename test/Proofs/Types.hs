{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -ddump-splices #-}
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

data U10 a = U10
  a
  a
  a
  a
  a
  a
  a
  a
  a
  a
  deriving (GHC.Generic, Show)

instance Generic (U10 a)
instance HasDatatypeInfo (U10 a)

data U10' a = U10'
  a
  a
  a
  a
  a
  a
  a
  a
  a
  a
  deriving (Show)

deriveGeneric ''U10'

data U50 a = U50
  a
  a
  a
  a
  a
  a
  a
  a
  a
  a
  a
  a
  a
  a
  a
  a
  a
  a
  a
  a
  a
  a
  a
  a
  a
  a
  a
  a
  a
  a
  a
  a
  a
  a
  a
  a
  a
  a
  a
  a
  a
  a
  a
  a
  a
  a
  a
  a
  a
  a
  deriving (GHC.Generic, Show)

instance Generic (U50 a)
instance HasDatatypeInfo (U50 a)

data U50' a = U50'
  a
  a
  a
  a
  a
  a
  a
  a
  a
  a
  a
  a
  a
  a
  a
  a
  a
  a
  a
  a
  a
  a
  a
  a
  a
  a
  a
  a
  a
  a
  a
  a
  a
  a
  a
  a
  a
  a
  a
  a
  a
  a
  a
  a
  a
  a
  a
  a
  a
  a
  deriving (Show)

deriveGeneric ''U50'

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

data E1 = E1_0
  deriving (GHC.Generic, Show)

instance Generic E1
instance HasDatatypeInfo E1

data E1' = E1'_0
  deriving (Show)

deriveGeneric ''E1'

data E2 = E2_0 | E2_1
  deriving (GHC.Generic, Show)

instance Generic E2
instance HasDatatypeInfo E2

data E2' = E2'_0 | E2'_1
  deriving (Show)

deriveGeneric ''E2'

data E3 = E3_0 | E3_1 | E3_2
  deriving (GHC.Generic, Show)

instance Generic E3
instance HasDatatypeInfo E3

data E3' = E3'_0 | E3'_1 | E3'_2
  deriving (Show)

deriveGeneric ''E3'

data E5 = E5_0 | E5_1 | E5_2 | E5_3 | E5_4
  deriving (GHC.Generic, Show)

instance Generic E5
instance HasDatatypeInfo E5

data E5' = E5'_0 | E5'_1 | E5'_2 | E5'_3 | E5'_4
  deriving (Show)

deriveGeneric ''E5'

data E6 = E6_0 | E6_1 | E6_2 | E6_3 | E6_4 | E6_5
  deriving (GHC.Generic, Show)

instance Generic E6
instance HasDatatypeInfo E6

data E6' = E6'_0 | E6'_1 | E6'_2 | E6'_3 | E6'_4 | E6'_5
  deriving (Show)

deriveGeneric ''E6'

data E10 = E10_0 | E10_1 | E10_2 | E10_3 | E10_4 | E10_5 | E10_6 | E10_7 | E10_8 | E10_9
  deriving (GHC.Generic, Show)

instance Generic E10
instance HasDatatypeInfo E10

data E10' = E10'_0 | E10'_1 | E10'_2 | E10'_3 | E10'_4 | E10'_5 | E10'_6 | E10'_7 | E10'_8 | E10'_9
  deriving (Show)

deriveGeneric ''E10'

data E50 =
    E50_00 | E50_01 | E50_02 | E50_03 | E50_04 | E50_05 | E50_06 | E50_07 | E50_08 | E50_09
  | E50_10 | E50_11 | E50_12 | E50_13 | E50_14 | E50_15 | E50_16 | E50_17 | E50_18 | E50_19
  | E50_20 | E50_21 | E50_22 | E50_23 | E50_24 | E50_25 | E50_26 | E50_27 | E50_28 | E50_29
  | E50_30 | E50_31 | E50_32 | E50_33 | E50_34 | E50_35 | E50_36 | E50_37 | E50_38 | E50_39
  | E50_40 | E50_41 | E50_42 | E50_43 | E50_44 | E50_45 | E50_46 | E50_47 | E50_48 | E50_49
  deriving (GHC.Generic, Show)

instance Generic E50
instance HasDatatypeInfo E50

data E50' =
    E50'_00 | E50'_01 | E50'_02 | E50'_03 | E50'_04 | E50'_05 | E50'_06 | E50'_07 | E50'_08 | E50'_09
  | E50'_10 | E50'_11 | E50'_12 | E50'_13 | E50'_14 | E50'_15 | E50'_16 | E50'_17 | E50'_18 | E50'_19
  | E50'_20 | E50'_21 | E50'_22 | E50'_23 | E50'_24 | E50'_25 | E50'_26 | E50'_27 | E50'_28 | E50'_29
  | E50'_30 | E50'_31 | E50'_32 | E50'_33 | E50'_34 | E50'_35 | E50'_36 | E50'_37 | E50'_38 | E50'_39
  | E50'_40 | E50'_41 | E50'_42 | E50'_43 | E50'_44 | E50'_45 | E50'_46 | E50'_47 | E50'_48 | E50'_49
  deriving (GHC.Generic, Show)

deriveGeneric ''E50'

data Tree =
    Leaf Int
  | Node Tree Tree
  deriving (GHC.Generic, Show)

instance Generic Tree
instance HasDatatypeInfo Tree

data Tree' =
    Leaf' Int
  | Node' Tree' Tree'
  deriving (GHC.Generic, Show)

deriveGeneric ''Tree'

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
