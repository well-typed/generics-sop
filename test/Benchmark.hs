{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeFamilies     #-}
module Main where

import Criterion
import Criterion.Main
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
