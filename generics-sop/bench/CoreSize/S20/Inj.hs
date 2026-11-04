{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
-- | The "CoreSize.S20.TH" datatype with a hand-written Generic instance that
-- injects through sop-core's shared 'injections' instead of writing out one
-- S-chain per constructor.
--
-- The injections are peeled off the product one 'tl' at a time into top-level
-- bindings, so each is selected once rather than on every call to 'from', and
-- the whole module stays linear in the constructor count. "SOPBench.Inject"
-- measures what the sharing costs at run time. "CoreSize.S20.InjLocal" is
-- the same idea without the top-level bindings: cheaper Core, far slower.
module CoreSize.S20.Inj where

import Generics.SOP

data S20 =
    S20_00
  | S20_01
  | S20_02
  | S20_03
  | S20_04
  | S20_05
  | S20_06
  | S20_07
  | S20_08
  | S20_09
  | S20_10
  | S20_11
  | S20_12
  | S20_13
  | S20_14
  | S20_15
  | S20_16
  | S20_17
  | S20_18
  | S20_19

type S20Code19 = '[ '[] ]
type S20Code18 = '[] ': S20Code19
type S20Code17 = '[] ': S20Code18
type S20Code16 = '[] ': S20Code17
type S20Code15 = '[] ': S20Code16
type S20Code14 = '[] ': S20Code15
type S20Code13 = '[] ': S20Code14
type S20Code12 = '[] ': S20Code13
type S20Code11 = '[] ': S20Code12
type S20Code10 = '[] ': S20Code11
type S20Code9 = '[] ': S20Code10
type S20Code8 = '[] ': S20Code9
type S20Code7 = '[] ': S20Code8
type S20Code6 = '[] ': S20Code7
type S20Code5 = '[] ': S20Code6
type S20Code4 = '[] ': S20Code5
type S20Code3 = '[] ': S20Code4
type S20Code2 = '[] ': S20Code3
type S20Code1 = '[] ': S20Code2
type S20Code0 = '[] ': S20Code1
type S20Code = S20Code0

rest00 :: NP (Injection (NP I) S20Code) S20Code0
rest00 = injections
rest01 :: NP (Injection (NP I) S20Code) S20Code1
rest01 = tl rest00
rest02 :: NP (Injection (NP I) S20Code) S20Code2
rest02 = tl rest01
rest03 :: NP (Injection (NP I) S20Code) S20Code3
rest03 = tl rest02
rest04 :: NP (Injection (NP I) S20Code) S20Code4
rest04 = tl rest03
rest05 :: NP (Injection (NP I) S20Code) S20Code5
rest05 = tl rest04
rest06 :: NP (Injection (NP I) S20Code) S20Code6
rest06 = tl rest05
rest07 :: NP (Injection (NP I) S20Code) S20Code7
rest07 = tl rest06
rest08 :: NP (Injection (NP I) S20Code) S20Code8
rest08 = tl rest07
rest09 :: NP (Injection (NP I) S20Code) S20Code9
rest09 = tl rest08
rest10 :: NP (Injection (NP I) S20Code) S20Code10
rest10 = tl rest09
rest11 :: NP (Injection (NP I) S20Code) S20Code11
rest11 = tl rest10
rest12 :: NP (Injection (NP I) S20Code) S20Code12
rest12 = tl rest11
rest13 :: NP (Injection (NP I) S20Code) S20Code13
rest13 = tl rest12
rest14 :: NP (Injection (NP I) S20Code) S20Code14
rest14 = tl rest13
rest15 :: NP (Injection (NP I) S20Code) S20Code15
rest15 = tl rest14
rest16 :: NP (Injection (NP I) S20Code) S20Code16
rest16 = tl rest15
rest17 :: NP (Injection (NP I) S20Code) S20Code17
rest17 = tl rest16
rest18 :: NP (Injection (NP I) S20Code) S20Code18
rest18 = tl rest17
rest19 :: NP (Injection (NP I) S20Code) S20Code19
rest19 = tl rest18

inj00 :: Injection (NP I) S20Code '[]
inj00 = hd rest00
inj01 :: Injection (NP I) S20Code '[]
inj01 = hd rest01
inj02 :: Injection (NP I) S20Code '[]
inj02 = hd rest02
inj03 :: Injection (NP I) S20Code '[]
inj03 = hd rest03
inj04 :: Injection (NP I) S20Code '[]
inj04 = hd rest04
inj05 :: Injection (NP I) S20Code '[]
inj05 = hd rest05
inj06 :: Injection (NP I) S20Code '[]
inj06 = hd rest06
inj07 :: Injection (NP I) S20Code '[]
inj07 = hd rest07
inj08 :: Injection (NP I) S20Code '[]
inj08 = hd rest08
inj09 :: Injection (NP I) S20Code '[]
inj09 = hd rest09
inj10 :: Injection (NP I) S20Code '[]
inj10 = hd rest10
inj11 :: Injection (NP I) S20Code '[]
inj11 = hd rest11
inj12 :: Injection (NP I) S20Code '[]
inj12 = hd rest12
inj13 :: Injection (NP I) S20Code '[]
inj13 = hd rest13
inj14 :: Injection (NP I) S20Code '[]
inj14 = hd rest14
inj15 :: Injection (NP I) S20Code '[]
inj15 = hd rest15
inj16 :: Injection (NP I) S20Code '[]
inj16 = hd rest16
inj17 :: Injection (NP I) S20Code '[]
inj17 = hd rest17
inj18 :: Injection (NP I) S20Code '[]
inj18 = hd rest18
inj19 :: Injection (NP I) S20Code '[]
inj19 = hd rest19

instance Generic S20 where
  type Code S20 = S20Code

  from :: S20 -> SOP I S20Code
  from S20_00 = SOP (unK (apFn inj00 Nil))
  from S20_01 = SOP (unK (apFn inj01 Nil))
  from S20_02 = SOP (unK (apFn inj02 Nil))
  from S20_03 = SOP (unK (apFn inj03 Nil))
  from S20_04 = SOP (unK (apFn inj04 Nil))
  from S20_05 = SOP (unK (apFn inj05 Nil))
  from S20_06 = SOP (unK (apFn inj06 Nil))
  from S20_07 = SOP (unK (apFn inj07 Nil))
  from S20_08 = SOP (unK (apFn inj08 Nil))
  from S20_09 = SOP (unK (apFn inj09 Nil))
  from S20_10 = SOP (unK (apFn inj10 Nil))
  from S20_11 = SOP (unK (apFn inj11 Nil))
  from S20_12 = SOP (unK (apFn inj12 Nil))
  from S20_13 = SOP (unK (apFn inj13 Nil))
  from S20_14 = SOP (unK (apFn inj14 Nil))
  from S20_15 = SOP (unK (apFn inj15 Nil))
  from S20_16 = SOP (unK (apFn inj16 Nil))
  from S20_17 = SOP (unK (apFn inj17 Nil))
  from S20_18 = SOP (unK (apFn inj18 Nil))
  from S20_19 = SOP (unK (apFn inj19 Nil))

  to :: SOP I S20Code -> S20
  to (SOP (Z Nil)) = S20_00
  to (SOP (S (Z Nil))) = S20_01
  to (SOP (S (S (Z Nil)))) = S20_02
  to (SOP (S (S (S (Z Nil))))) = S20_03
  to (SOP (S (S (S (S (Z Nil)))))) = S20_04
  to (SOP (S (S (S (S (S (Z Nil))))))) = S20_05
  to (SOP (S (S (S (S (S (S (Z Nil)))))))) = S20_06
  to (SOP (S (S (S (S (S (S (S (Z Nil))))))))) = S20_07
  to (SOP (S (S (S (S (S (S (S (S (Z Nil)))))))))) = S20_08
  to (SOP (S (S (S (S (S (S (S (S (S (Z Nil))))))))))) = S20_09
  to (SOP (S (S (S (S (S (S (S (S (S (S (Z Nil)))))))))))) = S20_10
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (Z Nil))))))))))))) = S20_11
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil)))))))))))))) = S20_12
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil))))))))))))))) = S20_13
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil)))))))))))))))) = S20_14
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil))))))))))))))))) = S20_15
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil)))))))))))))))))) = S20_16
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil))))))))))))))))))) = S20_17
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil)))))))))))))))))))) = S20_18
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil))))))))))))))))))))) = S20_19
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S x))))))))))))))))))))) = x `seq` error "inaccessible"
