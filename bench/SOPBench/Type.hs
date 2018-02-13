{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module SOPBench.Type where

import qualified Generics.SOP as SOP
import Generics.SOP.TH
import qualified GHC.Generics as GHC
import Language.Haskell.TH

import qualified SOPBench.Eq as SOP
import qualified SOPBench.Show as SOP

data Tree (tag :: Mode) =
    Leaf Int
  | Node (Tree tag) (Tree tag)

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


