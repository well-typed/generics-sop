{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
-- | As "CoreSize.S10.Inj", but destructuring sop-core's 'injections'
-- product inside 'from' rather than selecting out of it once at top level.
--
-- This is the cheaper of the two in Core, because nothing but the one
-- product is named, but it walks that product on every call, which
-- "SOPBench.Inject" shows costs roughly eightfold at run time.
module CoreSize.S10.InjLocal where

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

instance Generic S10 where
  type Code S10 = S10Code

  from :: S10 -> SOP I S10Code
  from x =
    case injections :: NP (Injection (NP I) S10Code) S10Code of
      i00 :* i01 :* i02 :* i03 :* i04 :* i05 :* i06 :* i07 :* i08 :* i09 :* Nil ->
        case x of
          S10_00 -> SOP (unK (apFn i00 Nil))
          S10_01 -> SOP (unK (apFn i01 Nil))
          S10_02 -> SOP (unK (apFn i02 Nil))
          S10_03 -> SOP (unK (apFn i03 Nil))
          S10_04 -> SOP (unK (apFn i04 Nil))
          S10_05 -> SOP (unK (apFn i05 Nil))
          S10_06 -> SOP (unK (apFn i06 Nil))
          S10_07 -> SOP (unK (apFn i07 Nil))
          S10_08 -> SOP (unK (apFn i08 Nil))
          S10_09 -> SOP (unK (apFn i09 Nil))

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
