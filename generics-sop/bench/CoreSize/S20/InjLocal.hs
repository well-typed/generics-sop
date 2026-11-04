{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
-- | As "CoreSize.S20.Inj", but destructuring sop-core's 'injections'
-- product inside 'from' rather than selecting out of it once at top level.
--
-- This is the cheaper of the two in Core, because nothing but the one
-- product is named, but it walks that product on every call, which
-- "SOPBench.Inject" shows costs roughly eightfold at run time.
module CoreSize.S20.InjLocal where

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

instance Generic S20 where
  type Code S20 = S20Code

  from :: S20 -> SOP I S20Code
  from x =
    case injections :: NP (Injection (NP I) S20Code) S20Code of
      i00 :* i01 :* i02 :* i03 :* i04 :* i05 :* i06 :* i07 :* i08 :* i09 :* i10 :* i11 :* i12 :* i13 :* i14 :* i15 :* i16 :* i17 :* i18 :* i19 :* Nil ->
        case x of
          S20_00 -> SOP (unK (apFn i00 Nil))
          S20_01 -> SOP (unK (apFn i01 Nil))
          S20_02 -> SOP (unK (apFn i02 Nil))
          S20_03 -> SOP (unK (apFn i03 Nil))
          S20_04 -> SOP (unK (apFn i04 Nil))
          S20_05 -> SOP (unK (apFn i05 Nil))
          S20_06 -> SOP (unK (apFn i06 Nil))
          S20_07 -> SOP (unK (apFn i07 Nil))
          S20_08 -> SOP (unK (apFn i08 Nil))
          S20_09 -> SOP (unK (apFn i09 Nil))
          S20_10 -> SOP (unK (apFn i10 Nil))
          S20_11 -> SOP (unK (apFn i11 Nil))
          S20_12 -> SOP (unK (apFn i12 Nil))
          S20_13 -> SOP (unK (apFn i13 Nil))
          S20_14 -> SOP (unK (apFn i14 Nil))
          S20_15 -> SOP (unK (apFn i15 Nil))
          S20_16 -> SOP (unK (apFn i16 Nil))
          S20_17 -> SOP (unK (apFn i17 Nil))
          S20_18 -> SOP (unK (apFn i18 Nil))
          S20_19 -> SOP (unK (apFn i19 Nil))

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
