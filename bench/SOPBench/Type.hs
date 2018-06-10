{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -ddump-simpl -dsuppress-all #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -funfolding-use-threshold=10000 #-}
module SOPBench.Type where

import Generics.SOP
import qualified Generics.SOP as SOP
import Generics.SOP.TH
import qualified GHC.Generics as GHC
import Language.Haskell.TH

import qualified SOPBench.Eq as SOP
import qualified SOPBench.Show as SOP

data S2 (tag :: Mode) =
    S2_0
  | S2_1

s2 :: S2 tag
s2 = S2_1

data S20 (tag :: Mode) =
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

s20 :: S20 tag
s20 = S20_17

data Tree (tag :: Mode) =
    Leaf Int
  | Node (Tree tag) (Tree tag)

tree :: Tree tag
tree = Node (Node (Leaf 1) (Leaf 2)) (Node (Leaf 3) (Leaf 4))

tree_medium :: Tree tag
tree_medium =
  Node (Node tree (Node tree tree)) (Node (Node tree tree) tree)

tree_large :: Tree tag
tree_large =
  Node
    (Node tree_medium (Node tree_medium tree_medium))
    (Node (Node tree_medium tree_medium) tree_medium)

data Prop (tag :: Mode) =
    Var String
  | T
  | F
  | Not (Prop tag)
  | And (Prop tag) (Prop tag)
  | Or  (Prop tag) (Prop tag)

data Mode =
    Handwritten
  | GHCDeriving
  | GHCGeneric
  | SOPGGP
  | SOPTH

deriving instance Eq                  (S2   'GHCDeriving)
deriving instance Show                (S2   'GHCDeriving)

deriving instance GHC.Generic         (S2   'GHCGeneric)
deriving instance GHC.Generic         (S2   'SOPGGP)
instance          SOP.Generic         (S2   'SOPGGP)
instance          SOP.HasDatatypeInfo (S2   'SOPGGP)

deriveGenericSubst ''S2 (const (promotedT 'SOPTH))

geq :: (Generic a, All2 Eq (Code a)) => a -> a -> Bool
geq = \ x y ->
  eq' (from x) (from y)
{-# INLINE geq #-}

eq' :: All2 Eq xss => SOP I xss -> SOP I xss -> Bool
eq' =
  ccompare_SOP
    peq
    False
    (\ x y -> and (hcollapse (hczipWith peq (mapIIK (==)) x y)))
    False
{-# INLINE eq' #-}

peq :: Proxy Eq
peq = Proxy

instance          Eq                  (S2   'SOPGGP) where
  (==) = geq

instance          Eq                  (S2   'SOPTH)  where
  (==) = geq

instance          Show                (S2   'SOPGGP) where
  showsPrec = SOP.gshowsPrec

instance          Show                (S2   'SOPTH)  where
  showsPrec = SOP.gshowsPrec

deriving instance Eq                  (S20  'GHCDeriving)
deriving instance Show                (S20  'GHCDeriving)

deriving instance GHC.Generic         (S20  'GHCGeneric)
deriving instance GHC.Generic         (S20  'SOPGGP)
instance          SOP.Generic         (S20  'SOPGGP)
instance          SOP.HasDatatypeInfo (S20  'SOPGGP)

deriveGenericSubst ''S20 (const (promotedT 'SOPTH))

instance          Eq                  (S20  'SOPGGP) where
  (==) = geq

instance          Eq                  (S20  'SOPTH)  where
  (==) = geq

instance          Show                (S20  'SOPGGP) where
  showsPrec = SOP.gshowsPrec

instance          Show                (S20  'SOPTH)  where
  showsPrec = SOP.gshowsPrec

deriving instance Eq                  (Tree 'GHCDeriving)
deriving instance Show                (Tree 'GHCDeriving)

deriving instance GHC.Generic         (Tree 'GHCGeneric)
deriving instance GHC.Generic         (Tree 'SOPGGP)
instance          SOP.Generic         (Tree 'SOPGGP)
instance          SOP.HasDatatypeInfo (Tree 'SOPGGP)

deriveGenericSubst ''Tree (const (promotedT 'SOPTH))

instance          Eq                  (Tree 'SOPGGP) where
  (==) = geq

instance          Eq                  (Tree 'SOPTH)  where
  (==) = geq

instance          Show                (Tree 'SOPGGP) where
  showsPrec = SOP.gshowsPrec

instance          Show                (Tree 'SOPTH)  where
  showsPrec = SOP.gshowsPrec

deriving instance Eq                  (Prop 'GHCDeriving)
deriving instance Show                (Prop 'GHCDeriving)

deriving instance GHC.Generic         (Prop 'GHCGeneric)
deriving instance GHC.Generic         (Prop 'SOPGGP)
instance          SOP.Generic         (Prop 'SOPGGP)
instance          SOP.HasDatatypeInfo (Prop 'SOPGGP)

deriveGenericSubst ''Prop (const (promotedT 'SOPTH))

instance          Eq                  (Prop 'SOPGGP) where
  (==) = SOP.geq

instance          Eq                  (Prop 'SOPTH)  where
  (==) = SOP.geq

instance          Show                (Prop 'SOPGGP) where
  showsPrec = SOP.gshowsPrec

instance          Show                (Prop 'SOPTH)  where
  showsPrec = SOP.gshowsPrec


