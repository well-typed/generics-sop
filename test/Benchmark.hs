{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
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
      [ bench "Template Haskell / combinator" $ nf show treeTHC
      , bench "Template Haskell" $ nf show treeTH
      , bench "Deriving Show"    $ nf show treeDer
      , bench "Hand-written"     $ nf show treeHW
      ]
    ]

instance NFData (TreeF a)
instance NFData TreeDer
instance NFData TreeTH
instance NFData TreeTHC
instance NFData TreeHW
instance NFData TreeGHC

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
