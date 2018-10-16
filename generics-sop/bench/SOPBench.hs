{-# LANGUAGE DataKinds #-}
module Main where

import Criterion.Main
import SOPBench.Type
import SOPBench.Roundtrip

main :: IO ()
main =
  defaultMainWith defaultConfig
    [ bgroup "Roundtrip"
      [ bench "S2 / GHCGeneric"     $ nf roundtrip (s2 :: S2 'GHCGeneric)
      , bench "S2 / SOPGGP"         $ nf roundtrip (s2 :: S2 'SOPGGP    )
      , bench "S2 / SOPTH"          $ nf roundtrip (s2 :: S2 'SOPTH     )
      , bench "S20 / GHCGeneric"    $ nf roundtrip (s20 :: S20 'GHCGeneric)
      , bench "S20 / SOPGGP"        $ nf roundtrip (s20 :: S20 'SOPGGP    )
      , bench "S20 / SOPTH"         $ nf roundtrip (s20 :: S20 'SOPTH     )
      , bench "PB2 / GHCGeneric"    $ nf roundtrip (pb2 :: PB2 'GHCGeneric)
      , bench "PB2 / SOPGGP"        $ nf roundtrip (pb2 :: PB2 'SOPGGP    )
      , bench "PB2 / SOPTH"         $ nf roundtrip (pb2 :: PB2 'SOPTH     )
      ]
    , bgroup "Eq"
      [ bench "S2 / GHCDeriving"           $ nf ((==) s2) (s2 :: S2 'GHCDeriving)
      , bench "S2 / SOPGGP"                $ nf ((==) s2) (s2 :: S2 'SOPGGP     )
      , bench "S2 / SOPTH"                 $ nf ((==) s2) (s2 :: S2 'SOPTH      )
      , bench "S20 / GHCDeriving"          $ nf ((==) s20) (s20 :: S20 'GHCDeriving)
      , bench "S20 / SOPGGP"               $ nf ((==) s20) (s20 :: S20 'SOPGGP     )
      , bench "S20 / SOPTH"                $ nf ((==) s20) (s20 :: S20 'SOPTH      )
      , bench "PB2 / GHCDeriving"          $ nf ((==) pb2) (pb2 :: PB2 'GHCDeriving)
      , bench "PB2 / SOPGGP"               $ nf ((==) pb2) (pb2 :: PB2 'SOPGGP     )
      , bench "PB2 / SOPTH"                $ nf ((==) pb2) (pb2 :: PB2 'SOPTH      )
      , bench "Tree / GHCDeriving"         $ nf ((==) tree) (tree :: Tree 'GHCDeriving)
      , bench "Tree / SOPGGP"              $ nf ((==) tree) (tree :: Tree 'SOPGGP     )
      , bench "Tree / SOPTH"               $ nf ((==) tree) (tree :: Tree 'SOPTH      )
      , bench "Tree large / GHCDeriving"   $ nf ((==) tree_large) (tree_large :: Tree 'GHCDeriving)
      , bench "Tree large / SOPGGP"        $ nf ((==) tree_large) (tree_large :: Tree 'SOPGGP     )
      , bench "Tree large / SOPTH"         $ nf ((==) tree_large) (tree_large :: Tree 'SOPTH      )
      ]
    , bgroup "Show"
      [ bench "S2 / GHCDeriving"         $ nf show (s2 :: S2 'GHCDeriving)
      , bench "S2 / SOPGGP"              $ nf show (s2 :: S2 'SOPGGP     )
      , bench "S2 / SOPTH"               $ nf show (s2 :: S2 'SOPTH      )
      , bench "S20 / GHCDeriving"        $ nf show (s20 :: S20 'GHCDeriving)
      , bench "S20 / SOPGGP"             $ nf show (s20 :: S20 'SOPGGP     )
      , bench "S20 / SOPTH"              $ nf show (s20 :: S20 'SOPTH      )
      , bench "PB2 / GHCDeriving"        $ nf show (pb2 :: PB2 'GHCDeriving)
      , bench "PB2 / SOPGGP"             $ nf show (pb2 :: PB2 'SOPGGP     )
      , bench "PB2 / SOPTH"              $ nf show (pb2 :: PB2 'SOPTH      )
      , bench "Tree / GHCDeriving"       $ nf show (tree :: Tree 'GHCDeriving)
      , bench "Tree / SOPGGP"            $ nf show (tree :: Tree 'SOPGGP     )
      , bench "Tree / SOPTH"             $ nf show (tree :: Tree 'SOPTH      )
      , bench "Tree large / GHCDeriving" $ nf show (tree_large :: Tree 'GHCDeriving)
      , bench "Tree large / SOPGGP"      $ nf show (tree_large :: Tree 'SOPGGP     )
      , bench "Tree large / SOPTH"       $ nf show (tree_large :: Tree 'SOPTH      )
      ]
    ]
