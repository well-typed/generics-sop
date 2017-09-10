{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
module TestInstances where

import qualified GHC.Generics as GHC
import Generics.SOP
import Generics.SOP.NS
import Generics.SOP.TH
import qualified Generics.SOP.Type.Metadata as T

-- Generic show, kind of
-- Explicitly recursive version (slow)
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

-- Combinator version (fast if optimised)
gshow' :: (Generic a, All2 Show (Code a)) => a -> String
gshow' =
    concat
  . hcollapse
  . hcmap (Proxy :: Proxy Show) (mapIK show)
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

-- GHC.Generics / explicitly recursive
data TreeB = LeafB Int | NodeB TreeB TreeB
  deriving (GHC.Generic)

treeB :: TreeB
treeB = NodeB (LeafB 1) (LeafB 2)

instance Generic TreeB
instance HasDatatypeInfo TreeB

instance Show TreeB where
  show = gshow

-- Template Haskell
data TreeC = LeafC Int | NodeC TreeC TreeC

treeC :: TreeC
treeC = NodeC (LeafC 1) (LeafC 2)

deriveGeneric ''TreeC

instance Show TreeC where
  show = gshow

-- Orphan approach
data TreeD = LeafD Int | NodeD TreeD TreeD
  deriving (GHC.Generic)

treeD :: TreeD
treeD = NodeD (LeafD 1) (LeafD 2)

deriveGenericFunctions ''TreeD "TreeDCode" "fromTreeD" "toTreeD"
deriveMetadataValue ''TreeD "TreeDCode" "treeDatatypeInfo"
deriveMetadataType ''TreeD "TreeDatatypeInfo"

demotedTreeDatatypeInfo :: DatatypeInfo TreeDCode
demotedTreeDatatypeInfo = T.demoteDatatypeInfo (Proxy :: Proxy TreeDatatypeInfo)

instance Show TreeD where
  show x = gshowS (fromTreeD x)

-- Deriving Show
data TreeE = LeafE Int | NodeE TreeE TreeE
  deriving (GHC.Generic, Show)

treeE :: TreeE
treeE = NodeE (LeafE 1) (LeafE 2)

instance Generic TreeE

-- hand-written
data TreeF = LeafF Int | NodeF TreeF TreeF

treeF :: TreeF
treeF = NodeF (LeafF 1) (LeafF 2)

instance Show TreeF where
  show (LeafF n) = show n
  show (NodeF t1 t2) = show t1 ++ show t2
