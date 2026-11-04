{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
-- | As "CoreSize.S80.Inj", but destructuring sop-core's 'injections'
-- product inside 'from' rather than selecting out of it once at top level.
--
-- This is the cheaper of the two in Core, because nothing but the one
-- product is named, but it walks that product on every call, which
-- "SOPBench.Inject" shows costs roughly eightfold at run time.
module CoreSize.S80.InjLocal where

import Generics.SOP

data S80 =
    S80_00
  | S80_01
  | S80_02
  | S80_03
  | S80_04
  | S80_05
  | S80_06
  | S80_07
  | S80_08
  | S80_09
  | S80_10
  | S80_11
  | S80_12
  | S80_13
  | S80_14
  | S80_15
  | S80_16
  | S80_17
  | S80_18
  | S80_19
  | S80_20
  | S80_21
  | S80_22
  | S80_23
  | S80_24
  | S80_25
  | S80_26
  | S80_27
  | S80_28
  | S80_29
  | S80_30
  | S80_31
  | S80_32
  | S80_33
  | S80_34
  | S80_35
  | S80_36
  | S80_37
  | S80_38
  | S80_39
  | S80_40
  | S80_41
  | S80_42
  | S80_43
  | S80_44
  | S80_45
  | S80_46
  | S80_47
  | S80_48
  | S80_49
  | S80_50
  | S80_51
  | S80_52
  | S80_53
  | S80_54
  | S80_55
  | S80_56
  | S80_57
  | S80_58
  | S80_59
  | S80_60
  | S80_61
  | S80_62
  | S80_63
  | S80_64
  | S80_65
  | S80_66
  | S80_67
  | S80_68
  | S80_69
  | S80_70
  | S80_71
  | S80_72
  | S80_73
  | S80_74
  | S80_75
  | S80_76
  | S80_77
  | S80_78
  | S80_79

type S80Code79 = '[ '[] ]
type S80Code78 = '[] ': S80Code79
type S80Code77 = '[] ': S80Code78
type S80Code76 = '[] ': S80Code77
type S80Code75 = '[] ': S80Code76
type S80Code74 = '[] ': S80Code75
type S80Code73 = '[] ': S80Code74
type S80Code72 = '[] ': S80Code73
type S80Code71 = '[] ': S80Code72
type S80Code70 = '[] ': S80Code71
type S80Code69 = '[] ': S80Code70
type S80Code68 = '[] ': S80Code69
type S80Code67 = '[] ': S80Code68
type S80Code66 = '[] ': S80Code67
type S80Code65 = '[] ': S80Code66
type S80Code64 = '[] ': S80Code65
type S80Code63 = '[] ': S80Code64
type S80Code62 = '[] ': S80Code63
type S80Code61 = '[] ': S80Code62
type S80Code60 = '[] ': S80Code61
type S80Code59 = '[] ': S80Code60
type S80Code58 = '[] ': S80Code59
type S80Code57 = '[] ': S80Code58
type S80Code56 = '[] ': S80Code57
type S80Code55 = '[] ': S80Code56
type S80Code54 = '[] ': S80Code55
type S80Code53 = '[] ': S80Code54
type S80Code52 = '[] ': S80Code53
type S80Code51 = '[] ': S80Code52
type S80Code50 = '[] ': S80Code51
type S80Code49 = '[] ': S80Code50
type S80Code48 = '[] ': S80Code49
type S80Code47 = '[] ': S80Code48
type S80Code46 = '[] ': S80Code47
type S80Code45 = '[] ': S80Code46
type S80Code44 = '[] ': S80Code45
type S80Code43 = '[] ': S80Code44
type S80Code42 = '[] ': S80Code43
type S80Code41 = '[] ': S80Code42
type S80Code40 = '[] ': S80Code41
type S80Code39 = '[] ': S80Code40
type S80Code38 = '[] ': S80Code39
type S80Code37 = '[] ': S80Code38
type S80Code36 = '[] ': S80Code37
type S80Code35 = '[] ': S80Code36
type S80Code34 = '[] ': S80Code35
type S80Code33 = '[] ': S80Code34
type S80Code32 = '[] ': S80Code33
type S80Code31 = '[] ': S80Code32
type S80Code30 = '[] ': S80Code31
type S80Code29 = '[] ': S80Code30
type S80Code28 = '[] ': S80Code29
type S80Code27 = '[] ': S80Code28
type S80Code26 = '[] ': S80Code27
type S80Code25 = '[] ': S80Code26
type S80Code24 = '[] ': S80Code25
type S80Code23 = '[] ': S80Code24
type S80Code22 = '[] ': S80Code23
type S80Code21 = '[] ': S80Code22
type S80Code20 = '[] ': S80Code21
type S80Code19 = '[] ': S80Code20
type S80Code18 = '[] ': S80Code19
type S80Code17 = '[] ': S80Code18
type S80Code16 = '[] ': S80Code17
type S80Code15 = '[] ': S80Code16
type S80Code14 = '[] ': S80Code15
type S80Code13 = '[] ': S80Code14
type S80Code12 = '[] ': S80Code13
type S80Code11 = '[] ': S80Code12
type S80Code10 = '[] ': S80Code11
type S80Code9 = '[] ': S80Code10
type S80Code8 = '[] ': S80Code9
type S80Code7 = '[] ': S80Code8
type S80Code6 = '[] ': S80Code7
type S80Code5 = '[] ': S80Code6
type S80Code4 = '[] ': S80Code5
type S80Code3 = '[] ': S80Code4
type S80Code2 = '[] ': S80Code3
type S80Code1 = '[] ': S80Code2
type S80Code0 = '[] ': S80Code1
type S80Code = S80Code0

instance Generic S80 where
  type Code S80 = S80Code

  from :: S80 -> SOP I S80Code
  from x =
    case injections :: NP (Injection (NP I) S80Code) S80Code of
      i00 :* i01 :* i02 :* i03 :* i04 :* i05 :* i06 :* i07 :* i08 :* i09 :* i10 :* i11 :* i12 :* i13 :* i14 :* i15 :* i16 :* i17 :* i18 :* i19 :* i20 :* i21 :* i22 :* i23 :* i24 :* i25 :* i26 :* i27 :* i28 :* i29 :* i30 :* i31 :* i32 :* i33 :* i34 :* i35 :* i36 :* i37 :* i38 :* i39 :* i40 :* i41 :* i42 :* i43 :* i44 :* i45 :* i46 :* i47 :* i48 :* i49 :* i50 :* i51 :* i52 :* i53 :* i54 :* i55 :* i56 :* i57 :* i58 :* i59 :* i60 :* i61 :* i62 :* i63 :* i64 :* i65 :* i66 :* i67 :* i68 :* i69 :* i70 :* i71 :* i72 :* i73 :* i74 :* i75 :* i76 :* i77 :* i78 :* i79 :* Nil ->
        case x of
          S80_00 -> SOP (unK (apFn i00 Nil))
          S80_01 -> SOP (unK (apFn i01 Nil))
          S80_02 -> SOP (unK (apFn i02 Nil))
          S80_03 -> SOP (unK (apFn i03 Nil))
          S80_04 -> SOP (unK (apFn i04 Nil))
          S80_05 -> SOP (unK (apFn i05 Nil))
          S80_06 -> SOP (unK (apFn i06 Nil))
          S80_07 -> SOP (unK (apFn i07 Nil))
          S80_08 -> SOP (unK (apFn i08 Nil))
          S80_09 -> SOP (unK (apFn i09 Nil))
          S80_10 -> SOP (unK (apFn i10 Nil))
          S80_11 -> SOP (unK (apFn i11 Nil))
          S80_12 -> SOP (unK (apFn i12 Nil))
          S80_13 -> SOP (unK (apFn i13 Nil))
          S80_14 -> SOP (unK (apFn i14 Nil))
          S80_15 -> SOP (unK (apFn i15 Nil))
          S80_16 -> SOP (unK (apFn i16 Nil))
          S80_17 -> SOP (unK (apFn i17 Nil))
          S80_18 -> SOP (unK (apFn i18 Nil))
          S80_19 -> SOP (unK (apFn i19 Nil))
          S80_20 -> SOP (unK (apFn i20 Nil))
          S80_21 -> SOP (unK (apFn i21 Nil))
          S80_22 -> SOP (unK (apFn i22 Nil))
          S80_23 -> SOP (unK (apFn i23 Nil))
          S80_24 -> SOP (unK (apFn i24 Nil))
          S80_25 -> SOP (unK (apFn i25 Nil))
          S80_26 -> SOP (unK (apFn i26 Nil))
          S80_27 -> SOP (unK (apFn i27 Nil))
          S80_28 -> SOP (unK (apFn i28 Nil))
          S80_29 -> SOP (unK (apFn i29 Nil))
          S80_30 -> SOP (unK (apFn i30 Nil))
          S80_31 -> SOP (unK (apFn i31 Nil))
          S80_32 -> SOP (unK (apFn i32 Nil))
          S80_33 -> SOP (unK (apFn i33 Nil))
          S80_34 -> SOP (unK (apFn i34 Nil))
          S80_35 -> SOP (unK (apFn i35 Nil))
          S80_36 -> SOP (unK (apFn i36 Nil))
          S80_37 -> SOP (unK (apFn i37 Nil))
          S80_38 -> SOP (unK (apFn i38 Nil))
          S80_39 -> SOP (unK (apFn i39 Nil))
          S80_40 -> SOP (unK (apFn i40 Nil))
          S80_41 -> SOP (unK (apFn i41 Nil))
          S80_42 -> SOP (unK (apFn i42 Nil))
          S80_43 -> SOP (unK (apFn i43 Nil))
          S80_44 -> SOP (unK (apFn i44 Nil))
          S80_45 -> SOP (unK (apFn i45 Nil))
          S80_46 -> SOP (unK (apFn i46 Nil))
          S80_47 -> SOP (unK (apFn i47 Nil))
          S80_48 -> SOP (unK (apFn i48 Nil))
          S80_49 -> SOP (unK (apFn i49 Nil))
          S80_50 -> SOP (unK (apFn i50 Nil))
          S80_51 -> SOP (unK (apFn i51 Nil))
          S80_52 -> SOP (unK (apFn i52 Nil))
          S80_53 -> SOP (unK (apFn i53 Nil))
          S80_54 -> SOP (unK (apFn i54 Nil))
          S80_55 -> SOP (unK (apFn i55 Nil))
          S80_56 -> SOP (unK (apFn i56 Nil))
          S80_57 -> SOP (unK (apFn i57 Nil))
          S80_58 -> SOP (unK (apFn i58 Nil))
          S80_59 -> SOP (unK (apFn i59 Nil))
          S80_60 -> SOP (unK (apFn i60 Nil))
          S80_61 -> SOP (unK (apFn i61 Nil))
          S80_62 -> SOP (unK (apFn i62 Nil))
          S80_63 -> SOP (unK (apFn i63 Nil))
          S80_64 -> SOP (unK (apFn i64 Nil))
          S80_65 -> SOP (unK (apFn i65 Nil))
          S80_66 -> SOP (unK (apFn i66 Nil))
          S80_67 -> SOP (unK (apFn i67 Nil))
          S80_68 -> SOP (unK (apFn i68 Nil))
          S80_69 -> SOP (unK (apFn i69 Nil))
          S80_70 -> SOP (unK (apFn i70 Nil))
          S80_71 -> SOP (unK (apFn i71 Nil))
          S80_72 -> SOP (unK (apFn i72 Nil))
          S80_73 -> SOP (unK (apFn i73 Nil))
          S80_74 -> SOP (unK (apFn i74 Nil))
          S80_75 -> SOP (unK (apFn i75 Nil))
          S80_76 -> SOP (unK (apFn i76 Nil))
          S80_77 -> SOP (unK (apFn i77 Nil))
          S80_78 -> SOP (unK (apFn i78 Nil))
          S80_79 -> SOP (unK (apFn i79 Nil))

  to :: SOP I S80Code -> S80
  to (SOP (Z Nil)) = S80_00
  to (SOP (S (Z Nil))) = S80_01
  to (SOP (S (S (Z Nil)))) = S80_02
  to (SOP (S (S (S (Z Nil))))) = S80_03
  to (SOP (S (S (S (S (Z Nil)))))) = S80_04
  to (SOP (S (S (S (S (S (Z Nil))))))) = S80_05
  to (SOP (S (S (S (S (S (S (Z Nil)))))))) = S80_06
  to (SOP (S (S (S (S (S (S (S (Z Nil))))))))) = S80_07
  to (SOP (S (S (S (S (S (S (S (S (Z Nil)))))))))) = S80_08
  to (SOP (S (S (S (S (S (S (S (S (S (Z Nil))))))))))) = S80_09
  to (SOP (S (S (S (S (S (S (S (S (S (S (Z Nil)))))))))))) = S80_10
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (Z Nil))))))))))))) = S80_11
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil)))))))))))))) = S80_12
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil))))))))))))))) = S80_13
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil)))))))))))))))) = S80_14
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil))))))))))))))))) = S80_15
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil)))))))))))))))))) = S80_16
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil))))))))))))))))))) = S80_17
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil)))))))))))))))))))) = S80_18
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil))))))))))))))))))))) = S80_19
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil)))))))))))))))))))))) = S80_20
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil))))))))))))))))))))))) = S80_21
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil)))))))))))))))))))))))) = S80_22
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil))))))))))))))))))))))))) = S80_23
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil)))))))))))))))))))))))))) = S80_24
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil))))))))))))))))))))))))))) = S80_25
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil)))))))))))))))))))))))))))) = S80_26
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil))))))))))))))))))))))))))))) = S80_27
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil)))))))))))))))))))))))))))))) = S80_28
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil))))))))))))))))))))))))))))))) = S80_29
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil)))))))))))))))))))))))))))))))) = S80_30
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil))))))))))))))))))))))))))))))))) = S80_31
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil)))))))))))))))))))))))))))))))))) = S80_32
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil))))))))))))))))))))))))))))))))))) = S80_33
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil)))))))))))))))))))))))))))))))))))) = S80_34
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil))))))))))))))))))))))))))))))))))))) = S80_35
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil)))))))))))))))))))))))))))))))))))))) = S80_36
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil))))))))))))))))))))))))))))))))))))))) = S80_37
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil)))))))))))))))))))))))))))))))))))))))) = S80_38
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil))))))))))))))))))))))))))))))))))))))))) = S80_39
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil)))))))))))))))))))))))))))))))))))))))))) = S80_40
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil))))))))))))))))))))))))))))))))))))))))))) = S80_41
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil)))))))))))))))))))))))))))))))))))))))))))) = S80_42
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil))))))))))))))))))))))))))))))))))))))))))))) = S80_43
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil)))))))))))))))))))))))))))))))))))))))))))))) = S80_44
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil))))))))))))))))))))))))))))))))))))))))))))))) = S80_45
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil)))))))))))))))))))))))))))))))))))))))))))))))) = S80_46
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil))))))))))))))))))))))))))))))))))))))))))))))))) = S80_47
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil)))))))))))))))))))))))))))))))))))))))))))))))))) = S80_48
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil))))))))))))))))))))))))))))))))))))))))))))))))))) = S80_49
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil)))))))))))))))))))))))))))))))))))))))))))))))))))) = S80_50
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil))))))))))))))))))))))))))))))))))))))))))))))))))))) = S80_51
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil)))))))))))))))))))))))))))))))))))))))))))))))))))))) = S80_52
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil))))))))))))))))))))))))))))))))))))))))))))))))))))))) = S80_53
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil)))))))))))))))))))))))))))))))))))))))))))))))))))))))) = S80_54
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil))))))))))))))))))))))))))))))))))))))))))))))))))))))))) = S80_55
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil)))))))))))))))))))))))))))))))))))))))))))))))))))))))))) = S80_56
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) = S80_57
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) = S80_58
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) = S80_59
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) = S80_60
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) = S80_61
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) = S80_62
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) = S80_63
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) = S80_64
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) = S80_65
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) = S80_66
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) = S80_67
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) = S80_68
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) = S80_69
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) = S80_70
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) = S80_71
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) = S80_72
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) = S80_73
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) = S80_74
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) = S80_75
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) = S80_76
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) = S80_77
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) = S80_78
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) = S80_79
  to (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S x))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) = x `seq` error "inaccessible"
