{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Proofs.Metadata where

import Generics.SOP

datatypeNameOf :: HasDatatypeInfo a => proxy a -> DatatypeName
datatypeNameOf = datatypeName . datatypeInfo
{-# INLINE datatypeNameOf #-}

constructorNames :: (SListI (Code a), HasDatatypeInfo a) => proxy a -> [ConstructorName]
constructorNames =
  hcollapse . constructorNames'
{-# INLINE constructorNames #-}

constructorNames' :: (SListI (Code a), HasDatatypeInfo a) => proxy a -> NP (K ConstructorName) (Code a)
constructorNames' =
  hmap (K . constructorName) . constructorInfo . datatypeInfo
{-# INLINE constructorNames' #-}

theConstructor :: forall a . (Generic a, HasDatatypeInfo a) => a -> ConstructorName
theConstructor x =
  hcollapse (hzipWith const (constructorNames' (Proxy :: Proxy a)) (unSOP (from x)))
{-# INLINE theConstructor #-}

