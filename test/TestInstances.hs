{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module TestInstances where

import qualified GHC.Generics as GHC
import Generics.SOP
import Generics.SOP.NS
import Generics.SOP.TH
import qualified Generics.SOP.Type.Metadata as T


-- Generic show, kind of
gshow :: (Generic a, All2 Show (Code a)) => a -> String
gshow x = gshowS (from x)
{-# INLINE gshow #-}

gshowS :: (All2 Show xss) => SOP I xss -> String
gshowS (SOP (Z xs))  = gshowP xs
gshowS (SOP (S xss)) = gshowS (SOP xss)
{-# INLINE gshowS #-}

gshowP :: (All Show xs) => NP I xs -> String
gshowP Nil         = ""
gshowP (I x :* xs) = show x ++ (gshowP xs)
{-# INLINE gshowP #-}

-- Combinator version
gshow' :: (Generic a, All2 Show (Code a)) => a -> String
gshow' =
    concat
  . collapse_SOP
  . cmap_SOP (Proxy :: Proxy Show) (mapIK show)
  . from
{-# INLINE gshow' #-}

-- GHC.Generics / combinator
data Tree = Leaf Int | Node Tree Tree
  deriving (GHC.Generic)

tree :: Tree
tree = Node (Leaf 1) (Leaf 2)

instance Generic Tree
instance HasDatatypeInfo Tree

instance Show Tree where
  show = gshow'

-- Template Haskell / combinator
data TreeA = LeafA Int | NodeA TreeA TreeA

treeA :: TreeA
treeA = NodeA (LeafA 1) (LeafA 2)

deriveGeneric ''TreeA

instance Show TreeA where
  show = gshow'

-- Template Haskell
data TreeB = LeafB Int | NodeB TreeB TreeB

treeB :: TreeB
treeB = NodeB (LeafB 1) (LeafB 2)

deriveGeneric ''TreeB

instance Show TreeB where
  show = gshow

-- Orphan approach
data TreeC = LeafC Int | NodeC TreeC TreeC
  deriving (GHC.Generic)

treeC :: TreeC
treeC = NodeC (LeafC 1) (LeafC 2)

deriveGenericFunctions ''TreeC "TreeCCode" "fromTreeC" "toTreeC"
deriveMetadataValue ''TreeC "TreeCCode" "treeDatatypeInfo"
deriveMetadataType ''TreeC "TreeDatatypeInfo"

demotedTreeDatatypeInfo :: DatatypeInfo TreeCCode
demotedTreeDatatypeInfo = T.demoteDatatypeInfo (Proxy :: Proxy TreeDatatypeInfo)

instance Show TreeC where
  show x = gshowS (fromTreeC x)

-- Deriving Show
data TreeD = LeafD Int | NodeD TreeD TreeD
  deriving (GHC.Generic, Show)

treeD :: TreeD
treeD = NodeD (LeafD 1) (LeafD 2)

instance Generic TreeD

-- hand-written
data TreeE = LeafE Int | NodeE TreeE TreeE

treeE :: TreeE
treeE = NodeE (LeafE 1) (LeafE 2)

instance Show TreeE where
  show (LeafE n) = show n
  show (NodeE t1 t2) = show t1 ++ show t2
