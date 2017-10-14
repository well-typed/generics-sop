{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Main where

import Control.DeepSeq
import Generics.SOP
import Data
import Weigh

import Test.QuickCheck hiding (sample)
import Test.QuickCheck.Gen hiding (sample)
import Test.QuickCheck.Arbitrary


main :: IO ()
main = do
  let samples :: forall (a :: GenericContext) . [TreeF a]
      samples = sample 10000 (arbitrary :: Gen (TreeF a))
  () <- return $ rnf samples

  mainWith $ do
    func "Template Haskell / combinator" show (TreeTHC <$> samples)
    func "Template Haskell" show (TreeTH  <$> samples)
    func "Derived"          show (TreeDer <$> samples)
    func "Hand-written"     show (TreeHW  <$> samples)


instance NFData (TreeF a)
instance NFData TreeDer
instance NFData TreeTH
instance NFData TreeTHC
instance NFData TreeHW
instance NFData TreeGHC
