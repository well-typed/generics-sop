{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module HTransExample where

import Generics.SOP

class IsTupleTypeOf xs y | xs -> y where
  toTuple :: NP I xs -> y
  default toTuple :: (Generic y, Code y ~ '[ xs ]) => NP I xs -> y
  toTuple = to . SOP . Z

instance IsTupleTypeOf '[] ()
instance IsTupleTypeOf '[x1] x1 where toTuple = unI . hd
instance IsTupleTypeOf '[x1, x2] (x1, x2)
instance IsTupleTypeOf '[x1, x2, x3] (x1, x2, x3)
instance IsTupleTypeOf '[x1, x2, x3, x4] (x1, x2, x3, x4)

convert :: (AllZip IsTupleTypeOf xss ys) => NS (NP I) xss -> NS I ys
convert = htrans (Proxy :: Proxy IsTupleTypeOf) (I . toTuple)

convertFull :: (Generic a, AllZip IsTupleTypeOf (Code a) ys) => a -> NS I ys
convertFull = convert . unSOP . from
