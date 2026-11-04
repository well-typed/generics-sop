{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
-- | The "CoreSize.S40.TH" datatype with a hand-written Generic instance that
-- injects through sop-core's shared 'injections' instead of writing out one
-- S-chain per constructor.
--
-- The injections are peeled off the product one 'tl' at a time into top-level
-- bindings, so each is selected once rather than on every call to 'from', and
-- the whole module stays linear in the constructor count. "SOPBench.Inject"
-- measures what the sharing costs at run time. "CoreSize.S40.InjLocal" is
-- the same idea without the top-level bindings: cheaper Core, far slower.
module CoreSize.S40.Inj where

import Generics.SOP

data S40 =
    S40_00
  | S40_01
  | S40_02
  | S40_03
  | S40_04
  | S40_05
  | S40_06
  | S40_07
  | S40_08
  | S40_09
  | S40_10
  | S40_11
  | S40_12
  | S40_13
  | S40_14
  | S40_15
  | S40_16
  | S40_17
  | S40_18
  | S40_19
  | S40_20
  | S40_21
  | S40_22
  | S40_23
  | S40_24
  | S40_25
  | S40_26
  | S40_27
  | S40_28
  | S40_29
  | S40_30
  | S40_31
  | S40_32
  | S40_33
  | S40_34
  | S40_35
  | S40_36
  | S40_37
  | S40_38
  | S40_39

type S40Code39 = '[ '[] ]
type S40Code38 = '[] ': S40Code39
type S40Code37 = '[] ': S40Code38
type S40Code36 = '[] ': S40Code37
type S40Code35 = '[] ': S40Code36
type S40Code34 = '[] ': S40Code35
type S40Code33 = '[] ': S40Code34
type S40Code32 = '[] ': S40Code33
type S40Code31 = '[] ': S40Code32
type S40Code30 = '[] ': S40Code31
type S40Code29 = '[] ': S40Code30
type S40Code28 = '[] ': S40Code29
type S40Code27 = '[] ': S40Code28
type S40Code26 = '[] ': S40Code27
type S40Code25 = '[] ': S40Code26
type S40Code24 = '[] ': S40Code25
type S40Code23 = '[] ': S40Code24
type S40Code22 = '[] ': S40Code23
type S40Code21 = '[] ': S40Code22
type S40Code20 = '[] ': S40Code21
type S40Code19 = '[] ': S40Code20
type S40Code18 = '[] ': S40Code19
type S40Code17 = '[] ': S40Code18
type S40Code16 = '[] ': S40Code17
type S40Code15 = '[] ': S40Code16
type S40Code14 = '[] ': S40Code15
type S40Code13 = '[] ': S40Code14
type S40Code12 = '[] ': S40Code13
type S40Code11 = '[] ': S40Code12
type S40Code10 = '[] ': S40Code11
type S40Code9 = '[] ': S40Code10
type S40Code8 = '[] ': S40Code9
type S40Code7 = '[] ': S40Code8
type S40Code6 = '[] ': S40Code7
type S40Code5 = '[] ': S40Code6
type S40Code4 = '[] ': S40Code5
type S40Code3 = '[] ': S40Code4
type S40Code2 = '[] ': S40Code3
type S40Code1 = '[] ': S40Code2
type S40Code0 = '[] ': S40Code1
type S40Code = S40Code0

rest00 :: NP (Injection (NP I) S40Code) S40Code0
rest00 = injections
rest01 :: NP (Injection (NP I) S40Code) S40Code1
rest01 = tl rest00
rest02 :: NP (Injection (NP I) S40Code) S40Code2
rest02 = tl rest01
rest03 :: NP (Injection (NP I) S40Code) S40Code3
rest03 = tl rest02
rest04 :: NP (Injection (NP I) S40Code) S40Code4
rest04 = tl rest03
rest05 :: NP (Injection (NP I) S40Code) S40Code5
rest05 = tl rest04
rest06 :: NP (Injection (NP I) S40Code) S40Code6
rest06 = tl rest05
rest07 :: NP (Injection (NP I) S40Code) S40Code7
rest07 = tl rest06
rest08 :: NP (Injection (NP I) S40Code) S40Code8
rest08 = tl rest07
rest09 :: NP (Injection (NP I) S40Code) S40Code9
rest09 = tl rest08
rest10 :: NP (Injection (NP I) S40Code) S40Code10
rest10 = tl rest09
rest11 :: NP (Injection (NP I) S40Code) S40Code11
rest11 = tl rest10
rest12 :: NP (Injection (NP I) S40Code) S40Code12
rest12 = tl rest11
rest13 :: NP (Injection (NP I) S40Code) S40Code13
rest13 = tl rest12
rest14 :: NP (Injection (NP I) S40Code) S40Code14
rest14 = tl rest13
rest15 :: NP (Injection (NP I) S40Code) S40Code15
rest15 = tl rest14
rest16 :: NP (Injection (NP I) S40Code) S40Code16
rest16 = tl rest15
rest17 :: NP (Injection (NP I) S40Code) S40Code17
rest17 = tl rest16
rest18 :: NP (Injection (NP I) S40Code) S40Code18
rest18 = tl rest17
rest19 :: NP (Injection (NP I) S40Code) S40Code19
rest19 = tl rest18
rest20 :: NP (Injection (NP I) S40Code) S40Code20
rest20 = tl rest19
rest21 :: NP (Injection (NP I) S40Code) S40Code21
rest21 = tl rest20
rest22 :: NP (Injection (NP I) S40Code) S40Code22
rest22 = tl rest21
rest23 :: NP (Injection (NP I) S40Code) S40Code23
rest23 = tl rest22
rest24 :: NP (Injection (NP I) S40Code) S40Code24
rest24 = tl rest23
rest25 :: NP (Injection (NP I) S40Code) S40Code25
rest25 = tl rest24
rest26 :: NP (Injection (NP I) S40Code) S40Code26
rest26 = tl rest25
rest27 :: NP (Injection (NP I) S40Code) S40Code27
rest27 = tl rest26
rest28 :: NP (Injection (NP I) S40Code) S40Code28
rest28 = tl rest27
rest29 :: NP (Injection (NP I) S40Code) S40Code29
rest29 = tl rest28
rest30 :: NP (Injection (NP I) S40Code) S40Code30
rest30 = tl rest29
rest31 :: NP (Injection (NP I) S40Code) S40Code31
rest31 = tl rest30
rest32 :: NP (Injection (NP I) S40Code) S40Code32
rest32 = tl rest31
rest33 :: NP (Injection (NP I) S40Code) S40Code33
rest33 = tl rest32
rest34 :: NP (Injection (NP I) S40Code) S40Code34
rest34 = tl rest33
rest35 :: NP (Injection (NP I) S40Code) S40Code35
rest35 = tl rest34
rest36 :: NP (Injection (NP I) S40Code) S40Code36
rest36 = tl rest35
rest37 :: NP (Injection (NP I) S40Code) S40Code37
rest37 = tl rest36
rest38 :: NP (Injection (NP I) S40Code) S40Code38
rest38 = tl rest37
rest39 :: NP (Injection (NP I) S40Code) S40Code39
rest39 = tl rest38

inj00 :: Injection (NP I) S40Code '[]
inj00 = hd rest00
inj01 :: Injection (NP I) S40Code '[]
inj01 = hd rest01
inj02 :: Injection (NP I) S40Code '[]
inj02 = hd rest02
inj03 :: Injection (NP I) S40Code '[]
inj03 = hd rest03
inj04 :: Injection (NP I) S40Code '[]
inj04 = hd rest04
inj05 :: Injection (NP I) S40Code '[]
inj05 = hd rest05
inj06 :: Injection (NP I) S40Code '[]
inj06 = hd rest06
inj07 :: Injection (NP I) S40Code '[]
inj07 = hd rest07
inj08 :: Injection (NP I) S40Code '[]
inj08 = hd rest08
inj09 :: Injection (NP I) S40Code '[]
inj09 = hd rest09
inj10 :: Injection (NP I) S40Code '[]
inj10 = hd rest10
inj11 :: Injection (NP I) S40Code '[]
inj11 = hd rest11
inj12 :: Injection (NP I) S40Code '[]
inj12 = hd rest12
inj13 :: Injection (NP I) S40Code '[]
inj13 = hd rest13
inj14 :: Injection (NP I) S40Code '[]
inj14 = hd rest14
inj15 :: Injection (NP I) S40Code '[]
inj15 = hd rest15
inj16 :: Injection (NP I) S40Code '[]
inj16 = hd rest16
inj17 :: Injection (NP I) S40Code '[]
inj17 = hd rest17
inj18 :: Injection (NP I) S40Code '[]
inj18 = hd rest18
inj19 :: Injection (NP I) S40Code '[]
inj19 = hd rest19
inj20 :: Injection (NP I) S40Code '[]
inj20 = hd rest20
inj21 :: Injection (NP I) S40Code '[]
inj21 = hd rest21
inj22 :: Injection (NP I) S40Code '[]
inj22 = hd rest22
inj23 :: Injection (NP I) S40Code '[]
inj23 = hd rest23
inj24 :: Injection (NP I) S40Code '[]
inj24 = hd rest24
inj25 :: Injection (NP I) S40Code '[]
inj25 = hd rest25
inj26 :: Injection (NP I) S40Code '[]
inj26 = hd rest26
inj27 :: Injection (NP I) S40Code '[]
inj27 = hd rest27
inj28 :: Injection (NP I) S40Code '[]
inj28 = hd rest28
inj29 :: Injection (NP I) S40Code '[]
inj29 = hd rest29
inj30 :: Injection (NP I) S40Code '[]
inj30 = hd rest30
inj31 :: Injection (NP I) S40Code '[]
inj31 = hd rest31
inj32 :: Injection (NP I) S40Code '[]
inj32 = hd rest32
inj33 :: Injection (NP I) S40Code '[]
inj33 = hd rest33
inj34 :: Injection (NP I) S40Code '[]
inj34 = hd rest34
inj35 :: Injection (NP I) S40Code '[]
inj35 = hd rest35
inj36 :: Injection (NP I) S40Code '[]
inj36 = hd rest36
inj37 :: Injection (NP I) S40Code '[]
inj37 = hd rest37
inj38 :: Injection (NP I) S40Code '[]
inj38 = hd rest38
inj39 :: Injection (NP I) S40Code '[]
inj39 = hd rest39

instance Generic S40 where
  type Code S40 = S40Code

  from :: S40 -> SOP I S40Code
  from S40_00 = SOP (unK (apFn inj00 Nil))
  from S40_01 = SOP (unK (apFn inj01 Nil))
  from S40_02 = SOP (unK (apFn inj02 Nil))
  from S40_03 = SOP (unK (apFn inj03 Nil))
  from S40_04 = SOP (unK (apFn inj04 Nil))
  from S40_05 = SOP (unK (apFn inj05 Nil))
  from S40_06 = SOP (unK (apFn inj06 Nil))
  from S40_07 = SOP (unK (apFn inj07 Nil))
  from S40_08 = SOP (unK (apFn inj08 Nil))
  from S40_09 = SOP (unK (apFn inj09 Nil))
  from S40_10 = SOP (unK (apFn inj10 Nil))
  from S40_11 = SOP (unK (apFn inj11 Nil))
  from S40_12 = SOP (unK (apFn inj12 Nil))
  from S40_13 = SOP (unK (apFn inj13 Nil))
  from S40_14 = SOP (unK (apFn inj14 Nil))
  from S40_15 = SOP (unK (apFn inj15 Nil))
  from S40_16 = SOP (unK (apFn inj16 Nil))
  from S40_17 = SOP (unK (apFn inj17 Nil))
  from S40_18 = SOP (unK (apFn inj18 Nil))
  from S40_19 = SOP (unK (apFn inj19 Nil))
  from S40_20 = SOP (unK (apFn inj20 Nil))
  from S40_21 = SOP (unK (apFn inj21 Nil))
  from S40_22 = SOP (unK (apFn inj22 Nil))
  from S40_23 = SOP (unK (apFn inj23 Nil))
  from S40_24 = SOP (unK (apFn inj24 Nil))
  from S40_25 = SOP (unK (apFn inj25 Nil))
  from S40_26 = SOP (unK (apFn inj26 Nil))
  from S40_27 = SOP (unK (apFn inj27 Nil))
  from S40_28 = SOP (unK (apFn inj28 Nil))
  from S40_29 = SOP (unK (apFn inj29 Nil))
  from S40_30 = SOP (unK (apFn inj30 Nil))
  from S40_31 = SOP (unK (apFn inj31 Nil))
  from S40_32 = SOP (unK (apFn inj32 Nil))
  from S40_33 = SOP (unK (apFn inj33 Nil))
  from S40_34 = SOP (unK (apFn inj34 Nil))
  from S40_35 = SOP (unK (apFn inj35 Nil))
  from S40_36 = SOP (unK (apFn inj36 Nil))
  from S40_37 = SOP (unK (apFn inj37 Nil))
  from S40_38 = SOP (unK (apFn inj38 Nil))
  from S40_39 = SOP (unK (apFn inj39 Nil))

  to :: SOP I S40Code -> S40
  to (SOP (Z Nil)) = S40_00
  to (SOP (S (Z Nil))) = S40_01
  to (SOP (S (S (Z Nil)))) = S40_02
  to (SOP (S (S (S (Z Nil))))) = S40_03
  to (SOP (S (S (S (S (Z Nil)))))) = S40_04
  to (SOP (S (S (S (S (S (Z Nil))))))) = S40_05
  to (SOP (S (S (S (S (S (S (Z Nil)))))))) = S40_06
  to (SOP (S (S (S (S (S (S (S (Z Nil))))))))) = S40_07
  to (SOP (S (S (S (S (S (S (S (S (Z Nil)))))))))) = S40_08
  to (SOP (S (S (S (S (S (S (S (S (S (Z Nil))))))))))) = S40_09
  to (SOP (S (S (S (S (S (S (S (S (S (S (Z Nil)))))))))))) = S40_10
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (Z Nil))))))))))))) = S40_11
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil)))))))))))))) = S40_12
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil))))))))))))))) = S40_13
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil)))))))))))))))) = S40_14
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil))))))))))))))))) = S40_15
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil)))))))))))))))))) = S40_16
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil))))))))))))))))))) = S40_17
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil)))))))))))))))))))) = S40_18
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil))))))))))))))))))))) = S40_19
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil)))))))))))))))))))))) = S40_20
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil))))))))))))))))))))))) = S40_21
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil)))))))))))))))))))))))) = S40_22
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil))))))))))))))))))))))))) = S40_23
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil)))))))))))))))))))))))))) = S40_24
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil))))))))))))))))))))))))))) = S40_25
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil)))))))))))))))))))))))))))) = S40_26
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil))))))))))))))))))))))))))))) = S40_27
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil)))))))))))))))))))))))))))))) = S40_28
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil))))))))))))))))))))))))))))))) = S40_29
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil)))))))))))))))))))))))))))))))) = S40_30
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil))))))))))))))))))))))))))))))))) = S40_31
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil)))))))))))))))))))))))))))))))))) = S40_32
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil))))))))))))))))))))))))))))))))))) = S40_33
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil)))))))))))))))))))))))))))))))))))) = S40_34
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil))))))))))))))))))))))))))))))))))))) = S40_35
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil)))))))))))))))))))))))))))))))))))))) = S40_36
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil))))))))))))))))))))))))))))))))))))))) = S40_37
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil)))))))))))))))))))))))))))))))))))))))) = S40_38
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil))))))))))))))))))))))))))))))))))))))))) = S40_39
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S x))))))))))))))))))))))))))))))))))))))))) = x `seq` error "inaccessible"
