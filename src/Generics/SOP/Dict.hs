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
-- This module is new and experimental in generics-sop 0.2.
-- It is therefore not yet exported via the main module and
-- has to be imported explicitly. Its interface is to be
-- considered even less stable than that of the rest of the
-- library. Feedback is very welcome though.
--
module Generics.SOP.Dict where

import Data.Proxy
import Generics.SOP.Classes
import Generics.SOP.Constraint
import Generics.SOP.NP
import Generics.SOP.Sing

data Dict (c :: k -> Constraint) (a :: k) where
  Dict :: c a => Dict c a

-- | A proof that the trivial constraint holds
-- over all type-level lists.
pureAll :: SListI xs => Dict (All Top) xs
pureAll = all_NP (hpure Dict)

-- | A proof that the trivial constraint holds
-- over all type-level lists of lists.
pureAll2 :: All SListI xss => Dict (All2 Top) xss
pureAll2 = all_POP (hpure Dict)

-- | Lifts a dictionary conversion over a type-level list.
mapAll :: forall c d xs .
          (forall a . Dict c a -> Dict d a)
       -> Dict (All c) xs -> Dict (All d) xs
mapAll f Dict = (all_NP . hmap f . unAll_NP) Dict

-- | Lifts a dictionary conversion over a type-level list
-- of lists.
mapAll2 :: forall c d xss .
           (forall a . Dict c a -> Dict d a)
        -> Dict (All2 c) xss -> Dict (All2 d) xss
mapAll2 f d @ Dict = (all2 . mapAll (mapAll f) . unAll2) d

-- | If two constraints 'c' and 'd' hold over a type-level
-- list 'xs', then the combination of both constraints holds
-- over that list.
zipAll :: Dict (All c) xs -> Dict (All d) xs -> Dict (All (c `Pair` d)) xs
zipAll dc @ Dict dd = all_NP (hzipWith (\ Dict Dict -> Dict) (unAll_NP dc) (unAll_NP dd))

-- | If two constraints 'c' and 'd' hold over a type-level
-- list of lists 'xss', then the combination of both constraints
-- holds over that list of lists.
zipAll2 :: All SListI xss => Dict (All2 c) xss -> Dict (All2 d) xss -> Dict (All2 (c `Pair` d)) xss
zipAll2 dc dd = all_POP (hzipWith (\ Dict Dict -> Dict) (unAll_POP dc) (unAll_POP dd))
-- TODO: I currently don't understand why the All constraint in the beginning
-- cannot be inferred.

-- | If we have a constraint 'c' that holds over a type-level
-- list 'xs', we can create a product containing proofs that
-- each individual list element satisfies 'c'.
unAll_NP :: forall c xs . Dict (All c) xs -> NP (Dict c) xs
unAll_NP Dict = hcpure (Proxy :: Proxy c) Dict

-- | If we have a constraint 'c' that holds over a type-level
-- list of lists 'xss', we can create a product of products
-- containing proofs that all the inner elements satisfy 'c'.
unAll_POP :: forall c xss . Dict (All2 c) xss -> POP (Dict c) xss
unAll_POP Dict = hcpure (Proxy :: Proxy c) Dict

-- | If we have a product containing proofs that each element
-- of 'xs' satisfies 'c', then 'All c' holds for 'xs'.
all_NP :: NP (Dict c) xs -> Dict (All c) xs
all_NP Nil          = Dict
all_NP (Dict :* ds) = withDict (all_NP ds) Dict

-- | If we have a product of products containing proofs that
-- each inner element of 'xss' satisfies 'c', then 'All2 c'
-- holds for 'xss'.
all_POP :: SListI xss => POP (Dict c) xss -> Dict (All2 c) xss
all_POP = all2 . all_NP . hmap all_NP . unPOP
-- TODO: Is the constraint necessary?

-- | The constraint 'All2 c' is convertible to 'All (All c)'.
unAll2 :: Dict (All2 c) xss -> Dict (All (All c)) xss
unAll2 Dict = Dict

-- | The constraint 'All (All c)' is convertible to 'All2 c'.
all2 :: Dict (All (All c)) xss -> Dict (All2 c) xss
all2 Dict = Dict

-- | If we have an explicit dictionary, we can unwrap it and
-- pass a function that makes use of it.
withDict :: Dict c a -> (c a => r) -> r
withDict Dict x = x
