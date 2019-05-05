{-# LANGUAGE DataKinds #-}
module Main where

import Criterion.Main
import SOPBench.Type
import SOPBench.Roundtrip

main :: IO ()
main =
  defaultMainWith defaultConfig
    [ bgroup "Roundtrip"
      [ bgroup "S2"
        [ bench "GHCGeneric"     $ nf roundtrip (s2 :: S2 'GHCGeneric)
        , bench "SOPGGP"         $ nf roundtrip (s2 :: S2 'SOPGGP    )
        , bench "SOPTH"          $ nf roundtrip (s2 :: S2 'SOPTH     )
        ]
      , bgroup "S20"
        [ bench "GHCGeneric"    $ nf roundtrip (s20 :: S20 'GHCGeneric)
        , bench "SOPGGP"        $ nf roundtrip (s20 :: S20 'SOPGGP    )
        , bench "SOPTH"         $ nf roundtrip (s20 :: S20 'SOPTH     )
        ]
      , bgroup "PB2"
        [ bench "GHCGeneric"    $ nf roundtrip (pb2 :: PB2 'GHCGeneric)
        , bench "SOPGGP"        $ nf roundtrip (pb2 :: PB2 'SOPGGP    )
        , bench "SOPTH"         $ nf roundtrip (pb2 :: PB2 'SOPTH     )
        ]
      ]
    , bgroup "Eq"
      [ bgroup "S2"
        [ bench "GHCDeriving"           $ nf ((==) s2) (s2 :: S2 'GHCDeriving)
        , bench "SOPGGP"                $ nf ((==) s2) (s2 :: S2 'SOPGGP     )
        , bench "SOPTH"                 $ nf ((==) s2) (s2 :: S2 'SOPTH      )
        ]
      , bgroup "S20"
        [ bench "GHCDeriving"          $ nf ((==) s20) (s20 :: S20 'GHCDeriving)
        , bench "SOPGGP"               $ nf ((==) s20) (s20 :: S20 'SOPGGP     )
        , bench "SOPTH"                $ nf ((==) s20) (s20 :: S20 'SOPTH      )
        ]
      , bgroup "PB2"
        [ bench "GHCDeriving"          $ nf ((==) pb2) (pb2 :: PB2 'GHCDeriving)
        , bench "SOPGGP"               $ nf ((==) pb2) (pb2 :: PB2 'SOPGGP     )
        , bench "SOPTH"                $ nf ((==) pb2) (pb2 :: PB2 'SOPTH      )
        ]
      , bgroup "Tree"
        [ bench "GHCDeriving"         $ nf ((==) tree) (tree :: Tree 'GHCDeriving)
        , bench "SOPGGP"              $ nf ((==) tree) (tree :: Tree 'SOPGGP     )
        , bench "SOPTH"               $ nf ((==) tree) (tree :: Tree 'SOPTH      )
        ]
      , bgroup "Tree large"
        [ bench "GHCDeriving"   $ nf ((==) tree_large) (tree_large :: Tree 'GHCDeriving)
        , bench "SOPGGP"        $ nf ((==) tree_large) (tree_large :: Tree 'SOPGGP     )
        , bench "SOPTH"         $ nf ((==) tree_large) (tree_large :: Tree 'SOPTH      )
        ]
      ]
    , bgroup "Show"
      [ bgroup "S2"
        [ bench "GHCDeriving"         $ nf show (s2 :: S2 'GHCDeriving)
        , bench "SOPGGP"              $ nf show (s2 :: S2 'SOPGGP     )
        , bench "SOPTH"               $ nf show (s2 :: S2 'SOPTH      )
        ]
      , bgroup "S20"
        [ bench "GHCDeriving"        $ nf show (s20 :: S20 'GHCDeriving)
        , bench "SOPGGP"             $ nf show (s20 :: S20 'SOPGGP     )
        , bench "SOPTH"              $ nf show (s20 :: S20 'SOPTH      )
        ]
      , bgroup "PB2"
        [ bench "GHCDeriving"        $ nf show (pb2 :: PB2 'GHCDeriving)
        , bench "SOPGGP"             $ nf show (pb2 :: PB2 'SOPGGP     )
        , bench "SOPTH"              $ nf show (pb2 :: PB2 'SOPTH      )
        ]
      , bgroup "Tree"
        [ bench "GHCDeriving"       $ nf show (tree :: Tree 'GHCDeriving)
        , bench "SOPGGP"            $ nf show (tree :: Tree 'SOPGGP     )
        , bench "SOPTH"             $ nf show (tree :: Tree 'SOPTH      )
        ]
      , bgroup "Tree large"
        [ bench "GHCDeriving" $ nf show (tree_large :: Tree 'GHCDeriving)
        , bench "SOPGGP"      $ nf show (tree_large :: Tree 'SOPGGP     )
        , bench "SOPTH"       $ nf show (tree_large :: Tree 'SOPTH      )
        ]
      ]
    ]
