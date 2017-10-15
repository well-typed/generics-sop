{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Main where

import Criterion
import Criterion.Main
import Generics.SOP.Benchmark.Bench


main :: IO ()
main = (suite :: IO Benchmark) >>= runBenchmark

instance Bench Benchmark where
  group        = bgroup
  item  n f x  = bench n $ nf f x
  runBenchmark = defaultMainWith defaultConfig . pure
