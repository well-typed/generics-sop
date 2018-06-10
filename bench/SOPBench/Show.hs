{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
module SOPBench.Show where

import Data.List (intersperse)
import Generics.SOP

gshow ::
  (Generic a, HasDatatypeInfo a, All2 Show (Code a)) => a -> String
gshow x =
  gshowsPrec 0 x ""

gshowsPrec ::
  (Generic a, HasDatatypeInfo a, All2 Show (Code a)) => Int -> a -> ShowS
gshowsPrec d x =
    hcollapse
  $ hczipWith pallshow (gshowsConstructor d)
      (constructorInfo (datatypeInfo (I x)))
      (unSOP (from x))

gshowsConstructor ::
  forall xs . (All Show xs) => Int -> ConstructorInfo xs -> NP I xs -> K ShowS xs
gshowsConstructor d i =
  case i of
    Constructor n -> \ x -> K
      $ showParen (d > app_prec)
      $ showString n . showString " " . gshowsConstructorArgs (app_prec + 1) x
    Infix n _ prec -> \ (I l :* I r :* Nil) -> K
      $ showParen (d > prec)
      $ showsPrec (prec + 1) l
      . showString " " . showString n . showString " "
      . showsPrec (prec + 1) r
    Record n fi -> \ x -> K
      $ showParen (d > app_prec) -- could be even higher, but seems to match GHC behaviour
      $ showString n . showString " {" . gshowsRecordArgs fi x . showString "}"

gshowsConstructorArgs ::
  (All Show xs) => Int -> NP I xs -> ShowS
gshowsConstructorArgs d x =
  foldr (.) id $ hcollapse $ hcmap pshow (K . showsPrec d . unI) x

gshowsRecordArgs ::
  (All Show xs) => NP FieldInfo xs -> NP I xs -> ShowS
gshowsRecordArgs fi x =
    foldr (.) id
  $ intersperse (showString ", ")
  $ hcollapse
  $ hczipWith pshow
      (\ (FieldInfo l) (I y) -> K (showString l . showString " = " . showsPrec 0 y))
      fi x

pallshow :: Proxy (All Show)
pallshow = Proxy

pshow :: Proxy Show
pshow = Proxy

app_prec :: Int
app_prec = 10
