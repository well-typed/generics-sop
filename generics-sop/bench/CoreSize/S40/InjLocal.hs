{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
-- | As "CoreSize.S40.Inj", but destructuring sop-core's 'injections'
-- product inside 'from' rather than selecting out of it once at top level.
--
-- This is the cheaper of the two in Core, because nothing but the one
-- product is named, but it walks that product on every call, which
-- "SOPBench.Inject" shows costs roughly eightfold at run time.
module CoreSize.S40.InjLocal where

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

instance Generic S40 where
  type Code S40 = S40Code

  from :: S40 -> SOP I S40Code
  from x =
    case injections :: NP (Injection (NP I) S40Code) S40Code of
      i00 :* i01 :* i02 :* i03 :* i04 :* i05 :* i06 :* i07 :* i08 :* i09 :* i10 :* i11 :* i12 :* i13 :* i14 :* i15 :* i16 :* i17 :* i18 :* i19 :* i20 :* i21 :* i22 :* i23 :* i24 :* i25 :* i26 :* i27 :* i28 :* i29 :* i30 :* i31 :* i32 :* i33 :* i34 :* i35 :* i36 :* i37 :* i38 :* i39 :* Nil ->
        case x of
          S40_00 -> SOP (unK (apFn i00 Nil))
          S40_01 -> SOP (unK (apFn i01 Nil))
          S40_02 -> SOP (unK (apFn i02 Nil))
          S40_03 -> SOP (unK (apFn i03 Nil))
          S40_04 -> SOP (unK (apFn i04 Nil))
          S40_05 -> SOP (unK (apFn i05 Nil))
          S40_06 -> SOP (unK (apFn i06 Nil))
          S40_07 -> SOP (unK (apFn i07 Nil))
          S40_08 -> SOP (unK (apFn i08 Nil))
          S40_09 -> SOP (unK (apFn i09 Nil))
          S40_10 -> SOP (unK (apFn i10 Nil))
          S40_11 -> SOP (unK (apFn i11 Nil))
          S40_12 -> SOP (unK (apFn i12 Nil))
          S40_13 -> SOP (unK (apFn i13 Nil))
          S40_14 -> SOP (unK (apFn i14 Nil))
          S40_15 -> SOP (unK (apFn i15 Nil))
          S40_16 -> SOP (unK (apFn i16 Nil))
          S40_17 -> SOP (unK (apFn i17 Nil))
          S40_18 -> SOP (unK (apFn i18 Nil))
          S40_19 -> SOP (unK (apFn i19 Nil))
          S40_20 -> SOP (unK (apFn i20 Nil))
          S40_21 -> SOP (unK (apFn i21 Nil))
          S40_22 -> SOP (unK (apFn i22 Nil))
          S40_23 -> SOP (unK (apFn i23 Nil))
          S40_24 -> SOP (unK (apFn i24 Nil))
          S40_25 -> SOP (unK (apFn i25 Nil))
          S40_26 -> SOP (unK (apFn i26 Nil))
          S40_27 -> SOP (unK (apFn i27 Nil))
          S40_28 -> SOP (unK (apFn i28 Nil))
          S40_29 -> SOP (unK (apFn i29 Nil))
          S40_30 -> SOP (unK (apFn i30 Nil))
          S40_31 -> SOP (unK (apFn i31 Nil))
          S40_32 -> SOP (unK (apFn i32 Nil))
          S40_33 -> SOP (unK (apFn i33 Nil))
          S40_34 -> SOP (unK (apFn i34 Nil))
          S40_35 -> SOP (unK (apFn i35 Nil))
          S40_36 -> SOP (unK (apFn i36 Nil))
          S40_37 -> SOP (unK (apFn i37 Nil))
          S40_38 -> SOP (unK (apFn i38 Nil))
          S40_39 -> SOP (unK (apFn i39 Nil))

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
