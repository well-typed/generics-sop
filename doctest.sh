#!/bin/sh

set -ex

doctest \
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
