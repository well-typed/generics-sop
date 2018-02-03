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

deriving instance Eq          (Tree 'GHCDeriving)
deriving instance Show        (Tree 'GHCDeriving)

deriving instance GHC.Generic (Tree 'GHCGeneric)
deriving instance GHC.Generic (Tree 'SOPGGP)
instance          SOP.Generic (Tree 'SOPGGP)

deriveGenericSubst ''Tree (const (promotedT 'SOPTH))

deriving instance Eq          (Prop 'GHCDeriving)
deriving instance Show        (Prop 'GHCDeriving)

deriving instance GHC.Generic (Prop 'GHCGeneric)
deriving instance GHC.Generic (Prop 'SOPGGP)
instance          SOP.Generic (Prop 'SOPGGP)

deriveGenericSubst ''Prop (const (promotedT 'SOPTH))


