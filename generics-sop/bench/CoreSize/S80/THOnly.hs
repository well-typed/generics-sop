{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
-- | As "CoreSize.S80.TH" but without the metadata instance, so that it is a
-- like-for-like baseline for "CoreSize.S80.Inj", which defines Generic only.
module CoreSize.S80.THOnly where

import Generics.SOP.TH

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

deriveGenericOnly ''S80
