{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Main (main, toTreeC) where

import qualified GHC.Generics as GHC
import Generics.SOP
import Generics.SOP.TH
import qualified Generics.SOP.Type.Metadata as T

import HTransExample

-- Generic show, kind of
gshow :: (Generic a, All2 Show (Code a)) => a -> String
gshow x = gshowS (from x)

gshowS :: (All2 Show xss) => SOP I xss -> String
gshowS (SOP (Z xs))  = gshowP xs
gshowS (SOP (S xss)) = gshowS (SOP xss)

gshowP :: (All Show xs) => NP I xs -> String
gshowP Nil         = ""
gshowP (I x :* xs) = show x ++ (gshowP xs)


-- GHC.Generics
data Tree = Leaf Int | Node Tree Tree
  deriving (GHC.Generic)

tree :: Tree
tree = Node (Leaf 1) (Leaf 2)

instance Generic Tree
instance HasDatatypeInfo Tree

instance Show Tree where
  show = gshow

-- Template Haskell
data TreeB = LeafB Int | NodeB TreeB TreeB

treeB :: TreeB
treeB = NodeB (LeafB 1) (LeafB 2)

deriveGeneric ''TreeB

instance Show TreeB where
  show = gshow

-- Orphan approach
data TreeC = LeafC Int | NodeC TreeC TreeC

treeC :: TreeC
treeC = NodeC (LeafC 1) (LeafC 2)

deriveGenericFunctions ''TreeC "TreeCCode" "fromTreeC" "toTreeC"
deriveMetadataValue ''TreeC "TreeCCode" "treeDatatypeInfo"
deriveMetadataType ''TreeC "TreeDatatypeInfo"

demotedTreeDatatypeInfo :: DatatypeInfo TreeCCode
demotedTreeDatatypeInfo = T.demoteDatatypeInfo (Proxy :: Proxy TreeDatatypeInfo)

instance Show TreeC where
  show x = gshowS (fromTreeC x)

-- Tests
main :: IO ()
main = do
  print tree
  print $ datatypeInfo (Proxy :: Proxy Tree)
  print treeB
  print $ datatypeInfo (Proxy :: Proxy TreeB)
  print treeC
  print treeDatatypeInfo
  print demotedTreeDatatypeInfo
  print (treeDatatypeInfo == demotedTreeDatatypeInfo)
  print $ convertFull tree
