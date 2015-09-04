{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Main (main) where

import qualified GHC.Generics as GHC
import Generics.SOP
import Generics.SOP.TH

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

instance Show Tree where
  show = gshow

-- Template Haskell
data TreeB = Leaf' Int | Node' TreeB TreeB

tree' :: TreeB
tree' = Node' (Leaf' 1) (Leaf' 2)

deriveGenericOnly ''TreeB

instance Show TreeB where
  show = gshow

-- Tests
main :: IO ()
main = do
  print tree
  print tree'
