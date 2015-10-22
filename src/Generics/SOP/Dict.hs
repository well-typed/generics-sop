{-# LANGUAGE PolyKinds #-}
-- | Explicit dictionaries.
--
-- When working with compound constraints such as constructed
-- using 'All' or 'All2', GHC cannot always prove automatically
-- what one would expect to hold.
--
-- This module provides a way of explicitly proving
-- conversions between such constraints to GHC. Such conversions
-- still have to be manually applied.
--
module Generics.SOP.Dict where

import Data.Proxy
import Generics.SOP.Classes
import Generics.SOP.Constraint
import Generics.SOP.NP
import Generics.SOP.Sing

data Dict (c :: k -> Constraint) (a :: k) where
  Dict :: c a => Dict c a

mapAll :: forall c d xs .
          (forall a . Dict c a -> Dict d a)
       -> Dict (All c) xs -> Dict (All d) xs
mapAll f Dict = (unDictAll . hmap f . dictAll) Dict

mapAll2 :: forall c d xss .
           (forall a . Dict c a -> Dict d a)
        -> Dict (All2 c) xss -> Dict (All2 d) xss
mapAll2 f Dict = (unDict2 . mapAll (mapAll f) . dict2) Dict

topAll :: SListI xs => Dict (All Top) xs
topAll = unDictAll (hpure Dict)

topAll2 :: All SListI xss => Dict (All2 Top) xss
topAll2 = unDictAll2 (hpure Dict)

pairAll :: Dict (All c) xs -> Dict (All d) xs -> Dict (All (c `Pair` d)) xs
pairAll dc @ Dict dd = unDictAll (hzipWith (\ Dict Dict -> Dict) (dictAll dc) (dictAll dd))

-- TODO: I currently don't understand why the All constraint in the beginning
-- cannot be inferred.
pairAll2 :: All SListI xss => Dict (All2 c) xss -> Dict (All2 d) xss -> Dict (All2 (c `Pair` d)) xss
pairAll2 dc dd = unDictAll2 (hzipWith (\ Dict Dict -> Dict) (dictAll2 dc) (dictAll2 dd))

dictAll :: forall c xs . Dict (All c) xs -> NP (Dict c) xs
dictAll Dict = hcpure (Proxy :: Proxy c) Dict

dictAll2 :: forall c xss . Dict (All2 c) xss -> POP (Dict c) xss
dictAll2 Dict = hcpure (Proxy :: Proxy c) Dict

unDictAll :: NP (Dict c) xs -> Dict (All c) xs
unDictAll Nil          = Dict
unDictAll (Dict :* ds) = (\ Dict -> Dict) (unDictAll ds)

unDictAll2 :: SListI xss => POP (Dict c) xss -> Dict (All2 c) xss
unDictAll2 = unDict2 . unDictAll . hmap unDictAll . unPOP

dict2 :: Dict (All2 c) xss -> Dict (All (All c)) xss
dict2 Dict = Dict

unDict2 :: Dict (All (All c)) xss -> Dict (All2 c) xss
unDict2 Dict = Dict
