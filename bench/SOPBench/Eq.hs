{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
module SOPBench.Eq where

import Generics.SOP

geq :: (Generic a, All2 Eq (Code a)) => a -> a -> Bool
geq = \ x y ->
  eq' (from x) (from y)
{-# INLINE geq #-}

eq' :: All2 Eq xss => SOP I xss -> SOP I xss -> Bool
eq' =
  ccompare_SOP
    peq
    False
    (\ x y -> and (hcollapse (hczipWith peq (mapIIK (==)) x y)))
    False
{-# INLINE eq' #-}

peq :: Proxy Eq
peq = Proxy
