{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module SOPBench.Type where

import Control.DeepSeq
import qualified Generics.SOP as SOP
import Generics.SOP.TH
import qualified GHC.Generics as GHC
import Language.Haskell.TH

import qualified SOPBench.Eq as SOP
import qualified SOPBench.Show as SOP
import SOPBench.Roundtrip

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

data PB2 (tag :: Mode) =
    PB2 Bool Bool

pb2 :: PB2 tag
pb2 = PB2 True False

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

-- NFData is used for forcing benchmark results, so we
-- derive it by hand for all variants of the datatype

rnfS2 :: S2 tag -> ()
rnfS2 S2_0 = ()
rnfS2 S2_1 = ()

instance          NFData              (S2   'GHCDeriving) where
  rnf = rnfS2

instance          NFData              (S2   'GHCGeneric ) where
  rnf = rnfS2

instance          NFData              (S2   'SOPGGP     ) where
  rnf = rnfS2

instance          NFData              (S2   'SOPTH      ) where
  rnf = rnfS2

rnfS20 :: S20 tag -> ()
rnfS20 S20_00 = ()
rnfS20 S20_01 = ()
rnfS20 S20_02 = ()
rnfS20 S20_03 = ()
rnfS20 S20_04 = ()
rnfS20 S20_05 = ()
rnfS20 S20_06 = ()
rnfS20 S20_07 = ()
rnfS20 S20_08 = ()
rnfS20 S20_09 = ()
rnfS20 S20_10 = ()
rnfS20 S20_11 = ()
rnfS20 S20_12 = ()
rnfS20 S20_13 = ()
rnfS20 S20_14 = ()
rnfS20 S20_15 = ()
rnfS20 S20_16 = ()
rnfS20 S20_17 = ()
rnfS20 S20_18 = ()
rnfS20 S20_19 = ()

instance          NFData              (S20  'GHCDeriving) where
  rnf = rnfS20

instance          NFData              (S20  'GHCGeneric ) where
  rnf = rnfS20

instance          NFData              (S20  'SOPGGP     ) where
  rnf = rnfS20

instance          NFData              (S20  'SOPTH      ) where
  rnf = rnfS20

rnfPB2 :: PB2 tag -> ()
rnfPB2 (PB2 b0 b1) =
  rnf b0 `seq` rnf b1

instance          NFData              (PB2  'GHCDeriving) where
  rnf = rnfPB2

instance          NFData              (PB2  'GHCGeneric ) where
  rnf = rnfPB2

instance          NFData              (PB2  'SOPGGP     ) where
  rnf = rnfPB2

instance          NFData              (PB2  'SOPTH      ) where
  rnf = rnfPB2

deriving instance Eq                  (S2   'GHCDeriving)
deriving instance Show                (S2   'GHCDeriving)

deriving instance GHC.Generic         (S2   'GHCGeneric)
deriving instance GHC.Generic         (S2   'SOPGGP)
instance          SOP.Generic         (S2   'SOPGGP)
instance          SOP.HasDatatypeInfo (S2   'SOPGGP)

deriveGenericSubst ''S2 (const (promotedT 'SOPTH))

instance          Roundtrip           (S2   'GHCGeneric) where
  roundtrip = ghcroundtrip

instance          Roundtrip           (S2   'SOPGGP) where
  roundtrip = soproundtrip

instance          Roundtrip           (S2   'SOPTH) where
  roundtrip = soproundtrip

instance          Eq                  (S2   'SOPGGP) where
  (==) = SOP.geq

instance          Eq                  (S2   'SOPTH)  where
  (==) = SOP.geq

instance          Show                (S2   'SOPGGP) where
  showsPrec = SOP.gshowsPrec

instance          Show                (S2   'SOPTH)  where
  showsPrec = SOP.gshowsPrec

deriveGenericSubst ''S20 (const (promotedT 'SOPTH))

instance          Roundtrip           (S20  'GHCGeneric) where
  roundtrip = ghcroundtrip

instance          Roundtrip           (S20  'SOPGGP) where
  roundtrip = soproundtrip

instance          Roundtrip           (S20  'SOPTH) where
  roundtrip = soproundtrip

deriving instance Eq                  (S20  'GHCDeriving)
deriving instance Show                (S20  'GHCDeriving)

deriving instance GHC.Generic         (S20  'GHCGeneric)
deriving instance GHC.Generic         (S20  'SOPGGP)
instance          SOP.Generic         (S20  'SOPGGP)
instance          SOP.HasDatatypeInfo (S20  'SOPGGP)

instance          Eq                  (S20  'SOPGGP) where
  (==) = SOP.geq

instance          Eq                  (S20  'SOPTH)  where
  (==) = SOP.geq

instance          Show                (S20  'SOPGGP) where
  showsPrec = SOP.gshowsPrec

instance          Show                (S20  'SOPTH)  where
  showsPrec = SOP.gshowsPrec

deriveGenericSubst ''PB2 (const (promotedT 'SOPTH))

instance          Roundtrip           (PB2  'GHCGeneric) where
  roundtrip = ghcroundtrip

instance          Roundtrip           (PB2  'SOPGGP) where
  roundtrip = soproundtrip

instance          Roundtrip           (PB2  'SOPTH) where
  roundtrip = soproundtrip

deriving instance Eq                  (PB2  'GHCDeriving)
deriving instance Show                (PB2  'GHCDeriving)

deriving instance GHC.Generic         (PB2  'GHCGeneric)
deriving instance GHC.Generic         (PB2  'SOPGGP)
instance          SOP.Generic         (PB2  'SOPGGP)
instance          SOP.HasDatatypeInfo (PB2  'SOPGGP)

instance          Eq                  (PB2  'SOPGGP) where
  (==) = SOP.geq

instance          Eq                  (PB2  'SOPTH) where
  (==) = SOP.geq

instance          Show                (PB2  'SOPGGP) where
  showsPrec = SOP.gshowsPrec

instance          Show                (PB2  'SOPTH) where
  showsPrec = SOP.gshowsPrec

deriving instance Eq                  (Tree 'GHCDeriving)
deriving instance Show                (Tree 'GHCDeriving)

deriving instance GHC.Generic         (Tree 'GHCGeneric)
deriving instance GHC.Generic         (Tree 'SOPGGP)
instance          SOP.Generic         (Tree 'SOPGGP)
instance          SOP.HasDatatypeInfo (Tree 'SOPGGP)

deriveGenericSubst ''Tree (const (promotedT 'SOPTH))

instance          Eq                  (Tree 'SOPGGP) where
  (==) = SOP.geq

instance          Eq                  (Tree 'SOPTH)  where
  (==) = SOP.geq

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


