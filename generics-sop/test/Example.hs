{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}
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

-- Generic enum, kind of
class Enumerable a where
  enum :: [a]

genum :: (Generic a, All2 Enumerable (Code a)) => [a]
genum =
  fmap to genumS

genumS :: (All SListI xss, All2 Enumerable xss) => [SOP I xss]
genumS =
  concat (fmap apInjs_POP
    (hsequence (hcpure (Proxy :: Proxy Enumerable) enum)))

-- GHC.Generics
data Tree = Leaf Int | Node Tree Tree
  deriving (GHC.Generic)

tree :: Tree
tree = Node (Leaf 1) (Leaf 2)

abc :: ABC
abc = B

instance Generic Tree
instance HasDatatypeInfo Tree

data ABC = A | B | C
  deriving (GHC.Generic)

instance Generic ABC
instance HasDatatypeInfo ABC

data Void
  deriving (GHC.Generic)

instance Generic Void
instance HasDatatypeInfo Void

instance Show Tree where
  show = gshow

instance Show ABC where
  show = gshow

instance Show Void where
  show = gshow

instance Enumerable ABC where
  enum = genum

instance Enumerable Void where
  enum = genum

-- Template Haskell
data TreeB = LeafB Int | NodeB TreeB TreeB

treeB :: TreeB
treeB = NodeB (LeafB 1) (LeafB 2)

deriveGeneric ''TreeB

data ABCB = AB | BB | CB

abcB :: ABCB
abcB = BB

deriveGeneric ''ABCB

data VoidB

deriveGeneric ''VoidB

instance Show TreeB where
  show = gshow

instance Show ABCB where
  show = gshow

instance Show VoidB where
  show = gshow

instance Enumerable ABCB where
  enum = genum

instance Enumerable VoidB where
  enum = genum

-- Orphan approach
data TreeC = LeafC Int | NodeC TreeC TreeC

treeC :: TreeC
treeC = NodeC (LeafC 1) (LeafC 2)

data ABCC = AC | BC | CC

abcC :: ABCC
abcC = BC

data VoidC

deriveGenericFunctions ''TreeC "TreeCCode" "fromTreeC" "toTreeC"
deriveMetadataValue ''TreeC "TreeCCode" "treeDatatypeInfo"
deriveMetadataType ''TreeC "TreeDatatypeInfo"

deriveGenericFunctions ''ABCC "ABCCCode" "fromABCC" "toABCC"
deriveMetadataValue ''ABCC "ABCCCode" "abcDatatypeInfo"
deriveMetadataType ''ABCC "ABCDatatypeInfo"

deriveGenericFunctions ''VoidC "VoidCCode" "fromVoidC" "toVoidC"
deriveMetadataValue ''VoidC "VoidCCode" "voidDatatypeInfo"
deriveMetadataType ''VoidC "VoidDatatypeInfo"

demotedTreeDatatypeInfo :: DatatypeInfo TreeCCode
demotedTreeDatatypeInfo = T.demoteDatatypeInfo (Proxy :: Proxy TreeDatatypeInfo)

demotedABCDatatypeInfo :: DatatypeInfo ABCCCode
demotedABCDatatypeInfo = T.demoteDatatypeInfo (Proxy :: Proxy ABCDatatypeInfo)

demotedVoidDatatypeInfo :: DatatypeInfo VoidCCode
demotedVoidDatatypeInfo = T.demoteDatatypeInfo (Proxy :: Proxy VoidDatatypeInfo)

instance Show TreeC where
  show x = gshowS (fromTreeC x)

instance Show ABCC where
  show x = gshowS (fromABCC x)

instance Show VoidC where
  show x = gshowS (fromVoidC x)

instance Enumerable ABCC where
  enum = fmap toABCC genumS

instance Enumerable VoidC where
  enum = fmap toVoidC genumS

-- Tests
main :: IO ()
main = do
  print tree
  print abc
  print $ (enum :: [ABC])
  print $ (enum :: [Void])
  print $ datatypeInfo (Proxy :: Proxy Tree)
  print $ datatypeInfo (Proxy :: Proxy Void)
  print treeB
  print abcB
  print $ (enum :: [ABCB])
  print $ (enum :: [VoidB])
  print $ datatypeInfo (Proxy :: Proxy TreeB)
  print $ datatypeInfo (Proxy :: Proxy VoidB)
  print treeC
  print abcC
  print $ (enum :: [ABCC])
  print $ (enum :: [VoidC])
  print treeDatatypeInfo
  print demotedTreeDatatypeInfo
  print (treeDatatypeInfo == demotedTreeDatatypeInfo)
  print (abcDatatypeInfo == demotedABCDatatypeInfo)
  print (voidDatatypeInfo == demotedVoidDatatypeInfo)
  print $ convertFull tree
