{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes     #-}
module Generics.SOP.Benchmark.Bench where

import Control.Applicative ((<$>))
import Control.DeepSeq
import Generics.SOP.Benchmark.Data
import Test.QuickCheck hiding (sample)
import Test.QuickCheck.Gen hiding (sample)
import Test.QuickCheck.Arbitrary


class Bench b where
  item         :: (NFData c) => String -> (a -> c) -> a -> b
  group        :: String -> [b] -> b
  runBenchmark :: b      -> IO ()


suite :: Bench b => IO b
suite = do
  let samples :: forall (a :: GenericContext) . [TreeF a]
      samples = sample 10000 (arbitrary :: Gen (TreeF a))
  () <- return $ rnf samples

  return $ group "SOP"
    [ group "Show"
      [ item "Template Haskell / combinator" show $ TreeSOPTHC <$> samples
      , item "Template Haskell" show $ TreeSOPTH  <$> samples
      , item "Derived"          show $ TreeSOPDer <$> samples
      , item "GHC Deriving"     show $ TreeGHCDer <$> samples
      , item "Hand-written"     show $ TreeHW     <$> samples
      ]
    , group "Eq"
      [ item "Derived"          eq $ TreeSOPDer <$> samples
      , item "Template Haskell" eq $ TreeSOPTH  <$> samples
      , item "GHC deriving"     eq $ TreeGHCDer <$> samples
      ]
    ]
  where
    eq x = x == x


instance NFData (TreeF a)
instance NFData TreeSOPDer
instance NFData TreeSOPTH
instance NFData TreeSOPTHC
instance NFData TreeHW
instance NFData TreeGHCGen
instance NFData TreeGHCDer
