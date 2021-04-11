module Main where

import Test.DocTest

main :: IO ()
main =
  doctest
    [ "--preserve-it"
    , "-XCPP"
    , "-XScopedTypeVariables"
    , "-XTypeFamilies"
    , "-XRankNTypes"
    , "-XTypeOperators"
    , "-XGADTs"
    , "-XConstraintKinds"
    , "-XMultiParamTypeClasses"
    , "-XTypeSynonymInstances"
    , "-XFlexibleInstances"
    , "-XFlexibleContexts"
    , "-XDeriveFunctor"
    , "-XDeriveFoldable"
    , "-XDeriveTraversable"
    , "-XDefaultSignatures"
    , "-XKindSignatures"
    , "-XDataKinds"
    , "-XFunctionalDependencies"
    , "--verbose"
    , "src"
    ]
