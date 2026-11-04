{-# LANGUAGE DataKinds #-}
-- | Runtime side of the CoreSize.*.Inj experiment.
--
-- Three ways of building the sum injection, against the Core each costs:
--
-- * TH       - one S-chain per constructor, as deriveGeneric writes them.
-- * Inj      - sop-core's shared 'injections', selected at top level.
-- * InjLocal - the same product, destructured inside 'from' on every call.
--
-- Each benchmark injects every constructor of the datatype once, to cover
-- the whole range of injection depths rather than just one of them.
module SOPBench.Inject (injectBenchmarks) where

import Criterion.Main
import Generics.SOP (from, hindex)

import qualified CoreSize.S20.TH as TH20
import qualified CoreSize.S20.Inj as Inj20
import qualified CoreSize.S20.InjLocal as InjLocal20
import qualified CoreSize.S80.TH as TH80
import qualified CoreSize.S80.Inj as Inj80
import qualified CoreSize.S80.InjLocal as InjLocal80

{-# NOINLINE th20 #-}
th20 :: [TH20.S20]
th20 =
  [ TH20.S20_00
  , TH20.S20_01
  , TH20.S20_02
  , TH20.S20_03
  , TH20.S20_04
  , TH20.S20_05
  , TH20.S20_06
  , TH20.S20_07
  , TH20.S20_08
  , TH20.S20_09
  , TH20.S20_10
  , TH20.S20_11
  , TH20.S20_12
  , TH20.S20_13
  , TH20.S20_14
  , TH20.S20_15
  , TH20.S20_16
  , TH20.S20_17
  , TH20.S20_18
  , TH20.S20_19
  ]

{-# NOINLINE inj20 #-}
inj20 :: [Inj20.S20]
inj20 =
  [ Inj20.S20_00
  , Inj20.S20_01
  , Inj20.S20_02
  , Inj20.S20_03
  , Inj20.S20_04
  , Inj20.S20_05
  , Inj20.S20_06
  , Inj20.S20_07
  , Inj20.S20_08
  , Inj20.S20_09
  , Inj20.S20_10
  , Inj20.S20_11
  , Inj20.S20_12
  , Inj20.S20_13
  , Inj20.S20_14
  , Inj20.S20_15
  , Inj20.S20_16
  , Inj20.S20_17
  , Inj20.S20_18
  , Inj20.S20_19
  ]

{-# NOINLINE injlocal20 #-}
injlocal20 :: [InjLocal20.S20]
injlocal20 =
  [ InjLocal20.S20_00
  , InjLocal20.S20_01
  , InjLocal20.S20_02
  , InjLocal20.S20_03
  , InjLocal20.S20_04
  , InjLocal20.S20_05
  , InjLocal20.S20_06
  , InjLocal20.S20_07
  , InjLocal20.S20_08
  , InjLocal20.S20_09
  , InjLocal20.S20_10
  , InjLocal20.S20_11
  , InjLocal20.S20_12
  , InjLocal20.S20_13
  , InjLocal20.S20_14
  , InjLocal20.S20_15
  , InjLocal20.S20_16
  , InjLocal20.S20_17
  , InjLocal20.S20_18
  , InjLocal20.S20_19
  ]

{-# NOINLINE th80 #-}
th80 :: [TH80.S80]
th80 =
  [ TH80.S80_00
  , TH80.S80_01
  , TH80.S80_02
  , TH80.S80_03
  , TH80.S80_04
  , TH80.S80_05
  , TH80.S80_06
  , TH80.S80_07
  , TH80.S80_08
  , TH80.S80_09
  , TH80.S80_10
  , TH80.S80_11
  , TH80.S80_12
  , TH80.S80_13
  , TH80.S80_14
  , TH80.S80_15
  , TH80.S80_16
  , TH80.S80_17
  , TH80.S80_18
  , TH80.S80_19
  , TH80.S80_20
  , TH80.S80_21
  , TH80.S80_22
  , TH80.S80_23
  , TH80.S80_24
  , TH80.S80_25
  , TH80.S80_26
  , TH80.S80_27
  , TH80.S80_28
  , TH80.S80_29
  , TH80.S80_30
  , TH80.S80_31
  , TH80.S80_32
  , TH80.S80_33
  , TH80.S80_34
  , TH80.S80_35
  , TH80.S80_36
  , TH80.S80_37
  , TH80.S80_38
  , TH80.S80_39
  , TH80.S80_40
  , TH80.S80_41
  , TH80.S80_42
  , TH80.S80_43
  , TH80.S80_44
  , TH80.S80_45
  , TH80.S80_46
  , TH80.S80_47
  , TH80.S80_48
  , TH80.S80_49
  , TH80.S80_50
  , TH80.S80_51
  , TH80.S80_52
  , TH80.S80_53
  , TH80.S80_54
  , TH80.S80_55
  , TH80.S80_56
  , TH80.S80_57
  , TH80.S80_58
  , TH80.S80_59
  , TH80.S80_60
  , TH80.S80_61
  , TH80.S80_62
  , TH80.S80_63
  , TH80.S80_64
  , TH80.S80_65
  , TH80.S80_66
  , TH80.S80_67
  , TH80.S80_68
  , TH80.S80_69
  , TH80.S80_70
  , TH80.S80_71
  , TH80.S80_72
  , TH80.S80_73
  , TH80.S80_74
  , TH80.S80_75
  , TH80.S80_76
  , TH80.S80_77
  , TH80.S80_78
  , TH80.S80_79
  ]

{-# NOINLINE inj80 #-}
inj80 :: [Inj80.S80]
inj80 =
  [ Inj80.S80_00
  , Inj80.S80_01
  , Inj80.S80_02
  , Inj80.S80_03
  , Inj80.S80_04
  , Inj80.S80_05
  , Inj80.S80_06
  , Inj80.S80_07
  , Inj80.S80_08
  , Inj80.S80_09
  , Inj80.S80_10
  , Inj80.S80_11
  , Inj80.S80_12
  , Inj80.S80_13
  , Inj80.S80_14
  , Inj80.S80_15
  , Inj80.S80_16
  , Inj80.S80_17
  , Inj80.S80_18
  , Inj80.S80_19
  , Inj80.S80_20
  , Inj80.S80_21
  , Inj80.S80_22
  , Inj80.S80_23
  , Inj80.S80_24
  , Inj80.S80_25
  , Inj80.S80_26
  , Inj80.S80_27
  , Inj80.S80_28
  , Inj80.S80_29
  , Inj80.S80_30
  , Inj80.S80_31
  , Inj80.S80_32
  , Inj80.S80_33
  , Inj80.S80_34
  , Inj80.S80_35
  , Inj80.S80_36
  , Inj80.S80_37
  , Inj80.S80_38
  , Inj80.S80_39
  , Inj80.S80_40
  , Inj80.S80_41
  , Inj80.S80_42
  , Inj80.S80_43
  , Inj80.S80_44
  , Inj80.S80_45
  , Inj80.S80_46
  , Inj80.S80_47
  , Inj80.S80_48
  , Inj80.S80_49
  , Inj80.S80_50
  , Inj80.S80_51
  , Inj80.S80_52
  , Inj80.S80_53
  , Inj80.S80_54
  , Inj80.S80_55
  , Inj80.S80_56
  , Inj80.S80_57
  , Inj80.S80_58
  , Inj80.S80_59
  , Inj80.S80_60
  , Inj80.S80_61
  , Inj80.S80_62
  , Inj80.S80_63
  , Inj80.S80_64
  , Inj80.S80_65
  , Inj80.S80_66
  , Inj80.S80_67
  , Inj80.S80_68
  , Inj80.S80_69
  , Inj80.S80_70
  , Inj80.S80_71
  , Inj80.S80_72
  , Inj80.S80_73
  , Inj80.S80_74
  , Inj80.S80_75
  , Inj80.S80_76
  , Inj80.S80_77
  , Inj80.S80_78
  , Inj80.S80_79
  ]

{-# NOINLINE injlocal80 #-}
injlocal80 :: [InjLocal80.S80]
injlocal80 =
  [ InjLocal80.S80_00
  , InjLocal80.S80_01
  , InjLocal80.S80_02
  , InjLocal80.S80_03
  , InjLocal80.S80_04
  , InjLocal80.S80_05
  , InjLocal80.S80_06
  , InjLocal80.S80_07
  , InjLocal80.S80_08
  , InjLocal80.S80_09
  , InjLocal80.S80_10
  , InjLocal80.S80_11
  , InjLocal80.S80_12
  , InjLocal80.S80_13
  , InjLocal80.S80_14
  , InjLocal80.S80_15
  , InjLocal80.S80_16
  , InjLocal80.S80_17
  , InjLocal80.S80_18
  , InjLocal80.S80_19
  , InjLocal80.S80_20
  , InjLocal80.S80_21
  , InjLocal80.S80_22
  , InjLocal80.S80_23
  , InjLocal80.S80_24
  , InjLocal80.S80_25
  , InjLocal80.S80_26
  , InjLocal80.S80_27
  , InjLocal80.S80_28
  , InjLocal80.S80_29
  , InjLocal80.S80_30
  , InjLocal80.S80_31
  , InjLocal80.S80_32
  , InjLocal80.S80_33
  , InjLocal80.S80_34
  , InjLocal80.S80_35
  , InjLocal80.S80_36
  , InjLocal80.S80_37
  , InjLocal80.S80_38
  , InjLocal80.S80_39
  , InjLocal80.S80_40
  , InjLocal80.S80_41
  , InjLocal80.S80_42
  , InjLocal80.S80_43
  , InjLocal80.S80_44
  , InjLocal80.S80_45
  , InjLocal80.S80_46
  , InjLocal80.S80_47
  , InjLocal80.S80_48
  , InjLocal80.S80_49
  , InjLocal80.S80_50
  , InjLocal80.S80_51
  , InjLocal80.S80_52
  , InjLocal80.S80_53
  , InjLocal80.S80_54
  , InjLocal80.S80_55
  , InjLocal80.S80_56
  , InjLocal80.S80_57
  , InjLocal80.S80_58
  , InjLocal80.S80_59
  , InjLocal80.S80_60
  , InjLocal80.S80_61
  , InjLocal80.S80_62
  , InjLocal80.S80_63
  , InjLocal80.S80_64
  , InjLocal80.S80_65
  , InjLocal80.S80_66
  , InjLocal80.S80_67
  , InjLocal80.S80_68
  , InjLocal80.S80_69
  , InjLocal80.S80_70
  , InjLocal80.S80_71
  , InjLocal80.S80_72
  , InjLocal80.S80_73
  , InjLocal80.S80_74
  , InjLocal80.S80_75
  , InjLocal80.S80_76
  , InjLocal80.S80_77
  , InjLocal80.S80_78
  , InjLocal80.S80_79
  ]

injectBenchmarks :: [Benchmark]
injectBenchmarks =
  [ bgroup "S20"
    [ bench "TH" $ nf (map (hindex . from)) th20
    , bench "Inj" $ nf (map (hindex . from)) inj20
    , bench "InjLocal" $ nf (map (hindex . from)) injlocal20
    ]
  , bgroup "S80"
    [ bench "TH" $ nf (map (hindex . from)) th80
    , bench "Inj" $ nf (map (hindex . from)) inj80
    , bench "InjLocal" $ nf (map (hindex . from)) injlocal80
    ]
  ]
