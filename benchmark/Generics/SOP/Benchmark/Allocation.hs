{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Main where

import Control.Applicative
import Control.DeepSeq
import Data.List (intercalate)
import Data.Tree
import Generics.SOP
import Generics.SOP.Benchmark.Bench
import Generics.SOP.Benchmark.Data
import Weigh

import Test.QuickCheck hiding (sample)
import Test.QuickCheck.Gen hiding (sample)
import Test.QuickCheck.Arbitrary


main :: IO ()
main = (suite :: IO WT) >>= runBenchmark


data WT where
  Item  :: (NFData c) => String -> (a -> c) -> a -> WT
  Group :: String -> [WT] -> WT

run :: WT -> IO ()
run = mainWith . go [] where
  go p (Item n f x) = func (intercalate " - " $ reverse (n:p)) f x
  go p (Group n xs) = sequence_ (map (go (n:p)) xs)

instance Bench WT where
  item  = Item
  group = Group
  runBenchmark = run
