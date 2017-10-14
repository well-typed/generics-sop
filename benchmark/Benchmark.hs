{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Main where

import Control.DeepSeq
import Criterion
import Criterion.Main
import Generics.SOP
import Data

import Test.QuickCheck hiding (sample)
import Test.QuickCheck.Gen hiding (sample)
import Test.QuickCheck.Arbitrary


main :: IO ()
main = do
  let samples :: forall (a :: GenericContext) . [TreeF a]
      samples = sample 10000 (arbitrary :: Gen (TreeF a))
  defaultMainWith defaultConfig
    [ bgroup "Show"
      [ bench "Template Haskell / combinator" . nf show $ TreeTHC <$> samples
      , bench "Template Haskell" . nf show $ TreeTH     <$> samples
      , bench "Derived"          . nf show $ TreeDer    <$> samples
      , bench "GHC Deriving"     . nf show $ TreeGHCDer <$> samples
      , bench "Hand-written"     . nf show $ TreeHW     <$> samples
      ]
    , bgroup "Eq"
      [ bench "Derived"          . nf eq $ TreeDer    <$> samples
      , bench "Template Haskell" . nf eq $ TreeTH     <$> samples
      , bench "GHC deriving"     . nf eq $ TreeGHCDer <$> samples
      ]
    ]
  where
    eq x = x == x

instance NFData (TreeF a)
instance NFData TreeDer
instance NFData TreeTH
instance NFData TreeTHC
instance NFData TreeHW
instance NFData TreeGHCGen
instance NFData TreeGHCDer

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
