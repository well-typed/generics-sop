{-# LANGUAGE DataKinds #-}
module Main where

import Criterion.Main
import SOPBench.Type

main :: IO ()
main =
  defaultMainWith defaultConfig
    [ bench "Eq / S2 / GHCDeriving"           $ nf ((==) s2) (s2 :: S2 'GHCDeriving)
    , bench "Eq / S2 / SOPGGP"                $ nf ((==) s2) (s2 :: S2 'SOPGGP     )
    , bench "Eq / S2 / SOPTH"                 $ nf ((==) s2) (s2 :: S2 'SOPTH      )
    , bench "Eq / S20 / GHCDeriving"          $ nf ((==) s20) (s20 :: S20 'GHCDeriving)
    , bench "Eq / S20 / SOPGGP"               $ nf ((==) s20) (s20 :: S20 'SOPGGP     )
    , bench "Eq / S20 / SOPTH"                $ nf ((==) s20) (s20 :: S20 'SOPTH      )
    , bench "Eq / Tree / GHCDeriving"         $ nf ((==) tree) (tree :: Tree 'GHCDeriving)
    , bench "Eq / Tree / SOPGGP"              $ nf ((==) tree) (tree :: Tree 'SOPGGP     )
    , bench "Eq / Tree / SOPTH"               $ nf ((==) tree) (tree :: Tree 'SOPTH      )
    , bench "Show / S2 / GHCDeriving"         $ nf show (s2 :: S2 'GHCDeriving)
    , bench "Show / S2 / SOPGGP"              $ nf show (s2 :: S2 'SOPGGP     )
    , bench "Show / S2 / SOPTH"               $ nf show (s2 :: S2 'SOPTH      )
    , bench "Show / S20 / GHCDeriving"        $ nf show (s20 :: S20 'GHCDeriving)
    , bench "Show / S20 / SOPGGP"             $ nf show (s20 :: S20 'SOPGGP     )
    , bench "Show / S20 / SOPTH"              $ nf show (s20 :: S20 'SOPTH      )
    , bench "Show / Tree / GHCDeriving"       $ nf show (tree :: Tree 'GHCDeriving)
    , bench "Show / Tree / SOPGGP"            $ nf show (tree :: Tree 'SOPGGP     )
    , bench "Show / Tree / SOPTH"             $ nf show (tree :: Tree 'SOPTH      )
    , bench "Show / Tree large / GHCDeriving" $ nf show (tree_large :: Tree 'GHCDeriving)
    , bench "Show / Tree large / SOPGGP"      $ nf show (tree_large :: Tree 'SOPGGP     )
    , bench "Show / Tree large / SOPTH"       $ nf show (tree_large :: Tree 'SOPTH      )
    ]
