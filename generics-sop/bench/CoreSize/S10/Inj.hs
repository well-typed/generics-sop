{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
-- | The "CoreSize.S10.TH" datatype with a hand-written Generic instance that
-- injects through sop-core's shared 'injections' instead of writing out one
-- S-chain per constructor.
--
-- The injections are peeled off the product one 'tl' at a time into top-level
-- bindings, so each is selected once rather than on every call to 'from', and
-- the whole module stays linear in the constructor count. "SOPBench.Inject"
-- measures what the sharing costs at run time. "CoreSize.S10.InjLocal" is
-- the same idea without the top-level bindings: cheaper Core, far slower.
module CoreSize.S10.Inj where

import Generics.SOP

data S10 =
    S10_00
  | S10_01
  | S10_02
  | S10_03
  | S10_04
  | S10_05
  | S10_06
  | S10_07
  | S10_08
  | S10_09

type S10Code9 = '[ '[] ]
type S10Code8 = '[] ': S10Code9
type S10Code7 = '[] ': S10Code8
type S10Code6 = '[] ': S10Code7
type S10Code5 = '[] ': S10Code6
type S10Code4 = '[] ': S10Code5
type S10Code3 = '[] ': S10Code4
type S10Code2 = '[] ': S10Code3
type S10Code1 = '[] ': S10Code2
type S10Code0 = '[] ': S10Code1
type S10Code = S10Code0

rest00 :: NP (Injection (NP I) S10Code) S10Code0
rest00 = injections
rest01 :: NP (Injection (NP I) S10Code) S10Code1
rest01 = tl rest00
rest02 :: NP (Injection (NP I) S10Code) S10Code2
rest02 = tl rest01
rest03 :: NP (Injection (NP I) S10Code) S10Code3
rest03 = tl rest02
rest04 :: NP (Injection (NP I) S10Code) S10Code4
rest04 = tl rest03
rest05 :: NP (Injection (NP I) S10Code) S10Code5
rest05 = tl rest04
rest06 :: NP (Injection (NP I) S10Code) S10Code6
rest06 = tl rest05
rest07 :: NP (Injection (NP I) S10Code) S10Code7
rest07 = tl rest06
rest08 :: NP (Injection (NP I) S10Code) S10Code8
rest08 = tl rest07
rest09 :: NP (Injection (NP I) S10Code) S10Code9
rest09 = tl rest08

inj00 :: Injection (NP I) S10Code '[]
inj00 = hd rest00
inj01 :: Injection (NP I) S10Code '[]
inj01 = hd rest01
inj02 :: Injection (NP I) S10Code '[]
inj02 = hd rest02
inj03 :: Injection (NP I) S10Code '[]
inj03 = hd rest03
inj04 :: Injection (NP I) S10Code '[]
inj04 = hd rest04
inj05 :: Injection (NP I) S10Code '[]
inj05 = hd rest05
inj06 :: Injection (NP I) S10Code '[]
inj06 = hd rest06
inj07 :: Injection (NP I) S10Code '[]
inj07 = hd rest07
inj08 :: Injection (NP I) S10Code '[]
inj08 = hd rest08
inj09 :: Injection (NP I) S10Code '[]
inj09 = hd rest09

instance Generic S10 where
  type Code S10 = S10Code

  from :: S10 -> SOP I S10Code
  from S10_00 = SOP (unK (apFn inj00 Nil))
  from S10_01 = SOP (unK (apFn inj01 Nil))
  from S10_02 = SOP (unK (apFn inj02 Nil))
  from S10_03 = SOP (unK (apFn inj03 Nil))
  from S10_04 = SOP (unK (apFn inj04 Nil))
  from S10_05 = SOP (unK (apFn inj05 Nil))
  from S10_06 = SOP (unK (apFn inj06 Nil))
  from S10_07 = SOP (unK (apFn inj07 Nil))
  from S10_08 = SOP (unK (apFn inj08 Nil))
  from S10_09 = SOP (unK (apFn inj09 Nil))

  to :: SOP I S10Code -> S10
  to (SOP (Z Nil)) = S10_00
  to (SOP (S (Z Nil))) = S10_01
  to (SOP (S (S (Z Nil)))) = S10_02
  to (SOP (S (S (S (Z Nil))))) = S10_03
  to (SOP (S (S (S (S (Z Nil)))))) = S10_04
  to (SOP (S (S (S (S (S (Z Nil))))))) = S10_05
  to (SOP (S (S (S (S (S (S (Z Nil)))))))) = S10_06
  to (SOP (S (S (S (S (S (S (S (Z Nil))))))))) = S10_07
  to (SOP (S (S (S (S (S (S (S (S (Z Nil)))))))))) = S10_08
  to (SOP (S (S (S (S (S (S (S (S (S (Z Nil))))))))))) = S10_09
  to (SOP (S (S (S (S (S (S (S (S (S (S x))))))))))) = x `seq` error "inaccessible"
