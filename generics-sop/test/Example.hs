{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}
module Main (main, toTreeC, toDataFamC) where

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

data family   DataFam a b c
data instance DataFam Int (Maybe b) c = DF b c
  deriving (GHC.Generic)

dataFam :: DataFam Int (Maybe Int) Int
dataFam = DF 1 2

instance Generic (DataFam Int (Maybe b) c)
instance HasDatatypeInfo (DataFam Int (Maybe b) c)

instance Show Tree where
  show = gshow

instance Show ABC where
  show = gshow

instance Show Void where
  show = gshow

instance (Show b, Show c) => Show (DataFam Int (Maybe b) c) where
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

data family   DataFamB a b c
data instance DataFamB Int (Maybe b) c = DFB b c

dataFamB :: DataFamB Int (Maybe Int) Int
dataFamB = DFB 1 2

deriveGeneric 'DFB

instance Show TreeB where
  show = gshow

instance Show ABCB where
  show = gshow

instance Show VoidB where
  show = gshow

instance (Show b, Show c) => Show (DataFamB Int (Maybe b) c) where
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

data family   DataFamC a b c
data instance DataFamC Int (Maybe b) c = DFC b c

dataFamC :: DataFamC Int (Maybe Int) Int
dataFamC = DFC 1 2

deriveGenericFunctions ''TreeC "TreeCCode" "fromTreeC" "toTreeC"
deriveMetadataValue ''TreeC "TreeCCode" "treeDatatypeInfo"
deriveMetadataType ''TreeC "TreeDatatypeInfo"

deriveGenericFunctions ''ABCC "ABCCCode" "fromABCC" "toABCC"
deriveMetadataValue ''ABCC "ABCCCode" "abcDatatypeInfo"
deriveMetadataType ''ABCC "ABCDatatypeInfo"

deriveGenericFunctions ''VoidC "VoidCCode" "fromVoidC" "toVoidC"
deriveMetadataValue ''VoidC "VoidCCode" "voidDatatypeInfo"
deriveMetadataType ''VoidC "VoidDatatypeInfo"

deriveGenericFunctions 'DFC "DataFamCCode" "fromDataFamC" "toDataFamC"
deriveMetadataValue 'DFC "DataFamCCode" "dataFamDatatypeInfo"
deriveMetadataType 'DFC "DataFamDatatypeInfo"

demotedTreeDatatypeInfo :: DatatypeInfo TreeCCode
demotedTreeDatatypeInfo = T.demoteDatatypeInfo (Proxy :: Proxy TreeDatatypeInfo)

demotedABCDatatypeInfo :: DatatypeInfo ABCCCode
demotedABCDatatypeInfo = T.demoteDatatypeInfo (Proxy :: Proxy ABCDatatypeInfo)

demotedVoidDatatypeInfo :: DatatypeInfo VoidCCode
demotedVoidDatatypeInfo = T.demoteDatatypeInfo (Proxy :: Proxy VoidDatatypeInfo)

demotedDataFamDatatypeInfo :: DatatypeInfo (DataFamCCode b c)
demotedDataFamDatatypeInfo = T.demoteDatatypeInfo (Proxy :: Proxy DataFamDatatypeInfo)

instance Show TreeC where
  show x = gshowS (fromTreeC x)

instance Show ABCC where
  show x = gshowS (fromABCC x)

instance Show VoidC where
  show x = gshowS (fromVoidC x)

instance (Show b, Show c) => Show (DataFamC Int (Maybe b) c) where
  show x = gshowS (fromDataFamC x)

instance Enumerable ABCC where
  enum = fmap toABCC genumS

instance Enumerable VoidC where
  enum = fmap toVoidC genumS

-- Use with derving via: much better than trying to write an overlapping
-- `(xs ~ SumCode a, IsSumType a xs, All Show xs) => Show a` instance
newtype AsSum a = AsSum a
instance (xs ~ SumCode a, IsSumType a xs, All Show xs) => Show (AsSum a) where
  show (AsSum a) = go @xs $ sumTypeTo a
    where 
      go :: (All Show xs') => NS I xs' -> String
      go (Z (I x)) = show x
      go (S xss)   = go xss

data UnionType = C1 Tree | C2 TreeB
  deriving stock (GHC.Generic)
  -- Use anyclass deriving via GHC generics to fit this all in one deriving clause
  deriving anyclass (Generic)
  deriving Show via (AsSum UnionType) 

-- Tests
main :: IO ()
main = do
  print tree
  print abc
  print dataFam
  print $ (enum :: [ABC])
  print $ (enum :: [Void])
  print $ datatypeInfo (Proxy :: Proxy Tree)
  print $ datatypeInfo (Proxy :: Proxy Void)
  print $ datatypeInfo (Proxy :: Proxy (DataFam Int (Maybe Int) Int))
  print treeB
  print abcB
  print dataFamB
  print $ (enum :: [ABCB])
  print $ (enum :: [VoidB])
  print $ datatypeInfo (Proxy :: Proxy TreeB)
  print $ datatypeInfo (Proxy :: Proxy VoidB)
  print $ datatypeInfo (Proxy :: Proxy (DataFamB Int (Maybe Int) Int))
  print treeC
  print abcC
  print dataFamC
  print $ (enum :: [ABCC])
  print $ (enum :: [VoidC])
  print treeDatatypeInfo
  print demotedTreeDatatypeInfo
  print demotedDataFamDatatypeInfo
  print (treeDatatypeInfo == demotedTreeDatatypeInfo)
  print (abcDatatypeInfo == demotedABCDatatypeInfo)
  print (voidDatatypeInfo == demotedVoidDatatypeInfo)
  print (dataFamDatatypeInfo == demotedDataFamDatatypeInfo)
  print $ convertFull tree
  print $ C1 $ Leaf 1
  print $ C2 $ LeafB 2
