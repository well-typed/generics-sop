{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeFamilies     #-}
module Main where

import Control.DeepSeq
import Criterion
import Criterion.Main
import Generics.SOP
import TestInstances


main :: IO ()
main = do
  defaultMainWith defaultConfig
    [ bgroup "Show"
      [ bench "GHC.Generics / combinator"     $ nf show tree
      , bench "Template Haskell / combinator" $ nf show treeA
      , bench "GHC.Generics / recursive"      $ nf show treeB
      , bench "Template Haskell / recursive " $ nf show treeC
      , bench "Orphan approach"  $ nf show treeD
      , bench "Deriving Show"    $ nf show treeE
      , bench "Hand-written"     $ nf show treeF
      ]
    ]

-- * NFData

grnf :: (Generic a, All2 NFData (Code a)) => a -> ()
grnf = grnfS . from

grnfS :: (All2 NFData xss) => SOP I xss -> ()
grnfS (SOP (Z xs))  = grnfP xs
grnfS (SOP (S xss)) = grnfS (SOP xss)

grnfP :: (All NFData xs) => NP I xs -> ()
grnfP Nil         = ()
grnfP (I x :* xs) = rnf x `seq` (grnfP xs)

instance NFData Tree  where rnf = grnf
instance NFData TreeA where rnf = grnf
instance NFData TreeB where rnf = grnf
instance NFData TreeC where rnf = grnf
instance NFData TreeD where rnf = grnfS . fromTreeD
instance NFData TreeE where rnf = grnf
instance NFData TreeF where
  rnf (LeafF x)   = rnf x
  rnf (NodeF l r) = rnf l `seq` rnf r

--instance GShow ListTHI where
--  show_ = gshow

{-
Data-types:
- Parametric and non-parametric
- Flat with many constructors
- Recursive
- CoRecursive
- Mutual-recursive
- Fractal (recursive with many constructos and shapes)

General:
 - show
 - read
 - traversable

List functions:
 - map
 - filter
 - foldl
 - foldr
 - unfold

Tree-Forest functions:
 - map
 - filter
 - fold
 - unfold
-}
