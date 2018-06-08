#!/bin/sh

set -ex

doctest --preserve-it \
  -XCPP \
  -XScopedTypeVariables \
  -XTypeFamilies \
  -XRankNTypes \
  -XTypeOperators \
  -XGADTs \
  -XConstraintKinds \
  -XMultiParamTypeClasses \
  -XTypeSynonymInstances \
  -XFlexibleInstances \
  -XFlexibleContexts \
  -XDeriveFunctor \
  -XDeriveFoldable \
  -XDeriveTraversable \
  -XDefaultSignatures \
  -XKindSignatures \
  -XDataKinds \
  -XFunctionalDependencies \
  $(find src -name '*.hs')
