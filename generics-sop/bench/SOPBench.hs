{-# LANGUAGE DataKinds #-}
module Main where

import Criterion.Main
import SOPBench.Type
import SOPBench.Roundtrip

main :: IO ()
main =
  defaultMainWith defaultConfig
    [ bench "Roundtrip / S2 / GHCGeneric"     $ nf roundtrip (s2 :: S2 'GHCGeneric)
    , bench "Roundtrip / S2 / SOPGGP"         $ nf roundtrip (s2 :: S2 'SOPGGP    )
    , bench "Roundtrip / S2 / SOPTH"          $ nf roundtrip (s2 :: S2 'SOPTH     )
    , bench "Roundtrip / S20 / GHCGeneric"    $ nf roundtrip (s20 :: S20 'GHCGeneric)
    , bench "Roundtrip / S20 / SOPGGP"        $ nf roundtrip (s20 :: S20 'SOPGGP    )
    , bench "Roundtrip / S20 / SOPTH"         $ nf roundtrip (s20 :: S20 'SOPTH     )
    , bench "Roundtrip / PB2 / GHCGeneric"    $ nf roundtrip (pb2 :: PB2 'GHCGeneric)
    , bench "Roundtrip / PB2 / SOPGGP"        $ nf roundtrip (pb2 :: PB2 'SOPGGP    )
    , bench "Roundtrip / PB2 / SOPTH"         $ nf roundtrip (pb2 :: PB2 'SOPTH     )
    , bench "Eq / S2 / GHCDeriving"           $ nf ((==) s2) (s2 :: S2 'GHCDeriving)
    , bench "Eq / S2 / SOPGGP"                $ nf ((==) s2) (s2 :: S2 'SOPGGP     )
    , bench "Eq / S2 / SOPTH"                 $ nf ((==) s2) (s2 :: S2 'SOPTH      )
    , bench "Eq / S20 / GHCDeriving"          $ nf ((==) s20) (s20 :: S20 'GHCDeriving)
    , bench "Eq / S20 / SOPGGP"               $ nf ((==) s20) (s20 :: S20 'SOPGGP     )
    , bench "Eq / S20 / SOPTH"                $ nf ((==) s20) (s20 :: S20 'SOPTH      )
    , bench "Eq / PB2 / GHCDeriving"          $ nf ((==) pb2) (pb2 :: PB2 'GHCDeriving)
    , bench "Eq / PB2 / SOPGGP"               $ nf ((==) pb2) (pb2 :: PB2 'SOPGGP     )
    , bench "Eq / PB2 / SOPTH"                $ nf ((==) pb2) (pb2 :: PB2 'SOPTH      )
    , bench "Eq / Tree / GHCDeriving"         $ nf ((==) tree) (tree :: Tree 'GHCDeriving)
    , bench "Eq / Tree / SOPGGP"              $ nf ((==) tree) (tree :: Tree 'SOPGGP     )
    , bench "Eq / Tree / SOPTH"               $ nf ((==) tree) (tree :: Tree 'SOPTH      )
    , bench "Eq / Tree large / GHCDeriving"   $ nf ((==) tree_large) (tree_large :: Tree 'GHCDeriving)
    , bench "Eq / Tree large / SOPGGP"        $ nf ((==) tree_large) (tree_large :: Tree 'SOPGGP     )
    , bench "Eq / Tree large / SOPTH"         $ nf ((==) tree_large) (tree_large :: Tree 'SOPTH      )
    , bench "Show / S2 / GHCDeriving"         $ nf show (s2 :: S2 'GHCDeriving)
    , bench "Show / S2 / SOPGGP"              $ nf show (s2 :: S2 'SOPGGP     )
    , bench "Show / S2 / SOPTH"               $ nf show (s2 :: S2 'SOPTH      )
    , bench "Show / S20 / GHCDeriving"        $ nf show (s20 :: S20 'GHCDeriving)
    , bench "Show / S20 / SOPGGP"             $ nf show (s20 :: S20 'SOPGGP     )
    , bench "Show / S20 / SOPTH"              $ nf show (s20 :: S20 'SOPTH      )
    , bench "Show / PB2 / GHCDeriving"        $ nf show (pb2 :: PB2 'GHCDeriving)
    , bench "Show / PB2 / SOPGGP"             $ nf show (pb2 :: PB2 'SOPGGP     )
    , bench "Show / PB2 / SOPTH"              $ nf show (pb2 :: PB2 'SOPTH      )
    , bench "Show / Tree / GHCDeriving"       $ nf show (tree :: Tree 'GHCDeriving)
    , bench "Show / Tree / SOPGGP"            $ nf show (tree :: Tree 'SOPGGP     )
    , bench "Show / Tree / SOPTH"             $ nf show (tree :: Tree 'SOPTH      )
    , bench "Show / Tree large / GHCDeriving" $ nf show (tree_large :: Tree 'GHCDeriving)
    , bench "Show / Tree large / SOPGGP"      $ nf show (tree_large :: Tree 'SOPGGP     )
    , bench "Show / Tree large / SOPTH"       $ nf show (tree_large :: Tree 'SOPTH      )
    ]
