{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
module SOPBench.Eq where

import Generics.SOP

geq :: (Generic a, All2 Eq (Code a)) => a -> a -> Bool
geq x y =
  eq' (from x) (from y)

eq' :: All2 Eq xss => SOP I xss -> SOP I xss -> Bool
eq' =
  ccompare_SOP
    peq
    False
    (\ x y -> and (hcollapse (hczipWith peq (mapIIK (==)) x y)))
    False

peq :: Proxy Eq
peq = Proxy
