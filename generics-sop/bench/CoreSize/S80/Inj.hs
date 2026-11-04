{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
-- | The "CoreSize.S80.TH" datatype with a hand-written Generic instance that
-- injects through sop-core's shared 'injections' instead of writing out one
-- S-chain per constructor.
--
-- The injections are peeled off the product one 'tl' at a time into top-level
-- bindings, so each is selected once rather than on every call to 'from', and
-- the whole module stays linear in the constructor count. "SOPBench.Inject"
-- measures what the sharing costs at run time. "CoreSize.S80.InjLocal" is
-- the same idea without the top-level bindings: cheaper Core, far slower.
module CoreSize.S80.Inj where

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

rest00 :: NP (Injection (NP I) S80Code) S80Code0
rest00 = injections
rest01 :: NP (Injection (NP I) S80Code) S80Code1
rest01 = tl rest00
rest02 :: NP (Injection (NP I) S80Code) S80Code2
rest02 = tl rest01
rest03 :: NP (Injection (NP I) S80Code) S80Code3
rest03 = tl rest02
rest04 :: NP (Injection (NP I) S80Code) S80Code4
rest04 = tl rest03
rest05 :: NP (Injection (NP I) S80Code) S80Code5
rest05 = tl rest04
rest06 :: NP (Injection (NP I) S80Code) S80Code6
rest06 = tl rest05
rest07 :: NP (Injection (NP I) S80Code) S80Code7
rest07 = tl rest06
rest08 :: NP (Injection (NP I) S80Code) S80Code8
rest08 = tl rest07
rest09 :: NP (Injection (NP I) S80Code) S80Code9
rest09 = tl rest08
rest10 :: NP (Injection (NP I) S80Code) S80Code10
rest10 = tl rest09
rest11 :: NP (Injection (NP I) S80Code) S80Code11
rest11 = tl rest10
rest12 :: NP (Injection (NP I) S80Code) S80Code12
rest12 = tl rest11
rest13 :: NP (Injection (NP I) S80Code) S80Code13
rest13 = tl rest12
rest14 :: NP (Injection (NP I) S80Code) S80Code14
rest14 = tl rest13
rest15 :: NP (Injection (NP I) S80Code) S80Code15
rest15 = tl rest14
rest16 :: NP (Injection (NP I) S80Code) S80Code16
rest16 = tl rest15
rest17 :: NP (Injection (NP I) S80Code) S80Code17
rest17 = tl rest16
rest18 :: NP (Injection (NP I) S80Code) S80Code18
rest18 = tl rest17
rest19 :: NP (Injection (NP I) S80Code) S80Code19
rest19 = tl rest18
rest20 :: NP (Injection (NP I) S80Code) S80Code20
rest20 = tl rest19
rest21 :: NP (Injection (NP I) S80Code) S80Code21
rest21 = tl rest20
rest22 :: NP (Injection (NP I) S80Code) S80Code22
rest22 = tl rest21
rest23 :: NP (Injection (NP I) S80Code) S80Code23
rest23 = tl rest22
rest24 :: NP (Injection (NP I) S80Code) S80Code24
rest24 = tl rest23
rest25 :: NP (Injection (NP I) S80Code) S80Code25
rest25 = tl rest24
rest26 :: NP (Injection (NP I) S80Code) S80Code26
rest26 = tl rest25
rest27 :: NP (Injection (NP I) S80Code) S80Code27
rest27 = tl rest26
rest28 :: NP (Injection (NP I) S80Code) S80Code28
rest28 = tl rest27
rest29 :: NP (Injection (NP I) S80Code) S80Code29
rest29 = tl rest28
rest30 :: NP (Injection (NP I) S80Code) S80Code30
rest30 = tl rest29
rest31 :: NP (Injection (NP I) S80Code) S80Code31
rest31 = tl rest30
rest32 :: NP (Injection (NP I) S80Code) S80Code32
rest32 = tl rest31
rest33 :: NP (Injection (NP I) S80Code) S80Code33
rest33 = tl rest32
rest34 :: NP (Injection (NP I) S80Code) S80Code34
rest34 = tl rest33
rest35 :: NP (Injection (NP I) S80Code) S80Code35
rest35 = tl rest34
rest36 :: NP (Injection (NP I) S80Code) S80Code36
rest36 = tl rest35
rest37 :: NP (Injection (NP I) S80Code) S80Code37
rest37 = tl rest36
rest38 :: NP (Injection (NP I) S80Code) S80Code38
rest38 = tl rest37
rest39 :: NP (Injection (NP I) S80Code) S80Code39
rest39 = tl rest38
rest40 :: NP (Injection (NP I) S80Code) S80Code40
rest40 = tl rest39
rest41 :: NP (Injection (NP I) S80Code) S80Code41
rest41 = tl rest40
rest42 :: NP (Injection (NP I) S80Code) S80Code42
rest42 = tl rest41
rest43 :: NP (Injection (NP I) S80Code) S80Code43
rest43 = tl rest42
rest44 :: NP (Injection (NP I) S80Code) S80Code44
rest44 = tl rest43
rest45 :: NP (Injection (NP I) S80Code) S80Code45
rest45 = tl rest44
rest46 :: NP (Injection (NP I) S80Code) S80Code46
rest46 = tl rest45
rest47 :: NP (Injection (NP I) S80Code) S80Code47
rest47 = tl rest46
rest48 :: NP (Injection (NP I) S80Code) S80Code48
rest48 = tl rest47
rest49 :: NP (Injection (NP I) S80Code) S80Code49
rest49 = tl rest48
rest50 :: NP (Injection (NP I) S80Code) S80Code50
rest50 = tl rest49
rest51 :: NP (Injection (NP I) S80Code) S80Code51
rest51 = tl rest50
rest52 :: NP (Injection (NP I) S80Code) S80Code52
rest52 = tl rest51
rest53 :: NP (Injection (NP I) S80Code) S80Code53
rest53 = tl rest52
rest54 :: NP (Injection (NP I) S80Code) S80Code54
rest54 = tl rest53
rest55 :: NP (Injection (NP I) S80Code) S80Code55
rest55 = tl rest54
rest56 :: NP (Injection (NP I) S80Code) S80Code56
rest56 = tl rest55
rest57 :: NP (Injection (NP I) S80Code) S80Code57
rest57 = tl rest56
rest58 :: NP (Injection (NP I) S80Code) S80Code58
rest58 = tl rest57
rest59 :: NP (Injection (NP I) S80Code) S80Code59
rest59 = tl rest58
rest60 :: NP (Injection (NP I) S80Code) S80Code60
rest60 = tl rest59
rest61 :: NP (Injection (NP I) S80Code) S80Code61
rest61 = tl rest60
rest62 :: NP (Injection (NP I) S80Code) S80Code62
rest62 = tl rest61
rest63 :: NP (Injection (NP I) S80Code) S80Code63
rest63 = tl rest62
rest64 :: NP (Injection (NP I) S80Code) S80Code64
rest64 = tl rest63
rest65 :: NP (Injection (NP I) S80Code) S80Code65
rest65 = tl rest64
rest66 :: NP (Injection (NP I) S80Code) S80Code66
rest66 = tl rest65
rest67 :: NP (Injection (NP I) S80Code) S80Code67
rest67 = tl rest66
rest68 :: NP (Injection (NP I) S80Code) S80Code68
rest68 = tl rest67
rest69 :: NP (Injection (NP I) S80Code) S80Code69
rest69 = tl rest68
rest70 :: NP (Injection (NP I) S80Code) S80Code70
rest70 = tl rest69
rest71 :: NP (Injection (NP I) S80Code) S80Code71
rest71 = tl rest70
rest72 :: NP (Injection (NP I) S80Code) S80Code72
rest72 = tl rest71
rest73 :: NP (Injection (NP I) S80Code) S80Code73
rest73 = tl rest72
rest74 :: NP (Injection (NP I) S80Code) S80Code74
rest74 = tl rest73
rest75 :: NP (Injection (NP I) S80Code) S80Code75
rest75 = tl rest74
rest76 :: NP (Injection (NP I) S80Code) S80Code76
rest76 = tl rest75
rest77 :: NP (Injection (NP I) S80Code) S80Code77
rest77 = tl rest76
rest78 :: NP (Injection (NP I) S80Code) S80Code78
rest78 = tl rest77
rest79 :: NP (Injection (NP I) S80Code) S80Code79
rest79 = tl rest78

inj00 :: Injection (NP I) S80Code '[]
inj00 = hd rest00
inj01 :: Injection (NP I) S80Code '[]
inj01 = hd rest01
inj02 :: Injection (NP I) S80Code '[]
inj02 = hd rest02
inj03 :: Injection (NP I) S80Code '[]
inj03 = hd rest03
inj04 :: Injection (NP I) S80Code '[]
inj04 = hd rest04
inj05 :: Injection (NP I) S80Code '[]
inj05 = hd rest05
inj06 :: Injection (NP I) S80Code '[]
inj06 = hd rest06
inj07 :: Injection (NP I) S80Code '[]
inj07 = hd rest07
inj08 :: Injection (NP I) S80Code '[]
inj08 = hd rest08
inj09 :: Injection (NP I) S80Code '[]
inj09 = hd rest09
inj10 :: Injection (NP I) S80Code '[]
inj10 = hd rest10
inj11 :: Injection (NP I) S80Code '[]
inj11 = hd rest11
inj12 :: Injection (NP I) S80Code '[]
inj12 = hd rest12
inj13 :: Injection (NP I) S80Code '[]
inj13 = hd rest13
inj14 :: Injection (NP I) S80Code '[]
inj14 = hd rest14
inj15 :: Injection (NP I) S80Code '[]
inj15 = hd rest15
inj16 :: Injection (NP I) S80Code '[]
inj16 = hd rest16
inj17 :: Injection (NP I) S80Code '[]
inj17 = hd rest17
inj18 :: Injection (NP I) S80Code '[]
inj18 = hd rest18
inj19 :: Injection (NP I) S80Code '[]
inj19 = hd rest19
inj20 :: Injection (NP I) S80Code '[]
inj20 = hd rest20
inj21 :: Injection (NP I) S80Code '[]
inj21 = hd rest21
inj22 :: Injection (NP I) S80Code '[]
inj22 = hd rest22
inj23 :: Injection (NP I) S80Code '[]
inj23 = hd rest23
inj24 :: Injection (NP I) S80Code '[]
inj24 = hd rest24
inj25 :: Injection (NP I) S80Code '[]
inj25 = hd rest25
inj26 :: Injection (NP I) S80Code '[]
inj26 = hd rest26
inj27 :: Injection (NP I) S80Code '[]
inj27 = hd rest27
inj28 :: Injection (NP I) S80Code '[]
inj28 = hd rest28
inj29 :: Injection (NP I) S80Code '[]
inj29 = hd rest29
inj30 :: Injection (NP I) S80Code '[]
inj30 = hd rest30
inj31 :: Injection (NP I) S80Code '[]
inj31 = hd rest31
inj32 :: Injection (NP I) S80Code '[]
inj32 = hd rest32
inj33 :: Injection (NP I) S80Code '[]
inj33 = hd rest33
inj34 :: Injection (NP I) S80Code '[]
inj34 = hd rest34
inj35 :: Injection (NP I) S80Code '[]
inj35 = hd rest35
inj36 :: Injection (NP I) S80Code '[]
inj36 = hd rest36
inj37 :: Injection (NP I) S80Code '[]
inj37 = hd rest37
inj38 :: Injection (NP I) S80Code '[]
inj38 = hd rest38
inj39 :: Injection (NP I) S80Code '[]
inj39 = hd rest39
inj40 :: Injection (NP I) S80Code '[]
inj40 = hd rest40
inj41 :: Injection (NP I) S80Code '[]
inj41 = hd rest41
inj42 :: Injection (NP I) S80Code '[]
inj42 = hd rest42
inj43 :: Injection (NP I) S80Code '[]
inj43 = hd rest43
inj44 :: Injection (NP I) S80Code '[]
inj44 = hd rest44
inj45 :: Injection (NP I) S80Code '[]
inj45 = hd rest45
inj46 :: Injection (NP I) S80Code '[]
inj46 = hd rest46
inj47 :: Injection (NP I) S80Code '[]
inj47 = hd rest47
inj48 :: Injection (NP I) S80Code '[]
inj48 = hd rest48
inj49 :: Injection (NP I) S80Code '[]
inj49 = hd rest49
inj50 :: Injection (NP I) S80Code '[]
inj50 = hd rest50
inj51 :: Injection (NP I) S80Code '[]
inj51 = hd rest51
inj52 :: Injection (NP I) S80Code '[]
inj52 = hd rest52
inj53 :: Injection (NP I) S80Code '[]
inj53 = hd rest53
inj54 :: Injection (NP I) S80Code '[]
inj54 = hd rest54
inj55 :: Injection (NP I) S80Code '[]
inj55 = hd rest55
inj56 :: Injection (NP I) S80Code '[]
inj56 = hd rest56
inj57 :: Injection (NP I) S80Code '[]
inj57 = hd rest57
inj58 :: Injection (NP I) S80Code '[]
inj58 = hd rest58
inj59 :: Injection (NP I) S80Code '[]
inj59 = hd rest59
inj60 :: Injection (NP I) S80Code '[]
inj60 = hd rest60
inj61 :: Injection (NP I) S80Code '[]
inj61 = hd rest61
inj62 :: Injection (NP I) S80Code '[]
inj62 = hd rest62
inj63 :: Injection (NP I) S80Code '[]
inj63 = hd rest63
inj64 :: Injection (NP I) S80Code '[]
inj64 = hd rest64
inj65 :: Injection (NP I) S80Code '[]
inj65 = hd rest65
inj66 :: Injection (NP I) S80Code '[]
inj66 = hd rest66
inj67 :: Injection (NP I) S80Code '[]
inj67 = hd rest67
inj68 :: Injection (NP I) S80Code '[]
inj68 = hd rest68
inj69 :: Injection (NP I) S80Code '[]
inj69 = hd rest69
inj70 :: Injection (NP I) S80Code '[]
inj70 = hd rest70
inj71 :: Injection (NP I) S80Code '[]
inj71 = hd rest71
inj72 :: Injection (NP I) S80Code '[]
inj72 = hd rest72
inj73 :: Injection (NP I) S80Code '[]
inj73 = hd rest73
inj74 :: Injection (NP I) S80Code '[]
inj74 = hd rest74
inj75 :: Injection (NP I) S80Code '[]
inj75 = hd rest75
inj76 :: Injection (NP I) S80Code '[]
inj76 = hd rest76
inj77 :: Injection (NP I) S80Code '[]
inj77 = hd rest77
inj78 :: Injection (NP I) S80Code '[]
inj78 = hd rest78
inj79 :: Injection (NP I) S80Code '[]
inj79 = hd rest79

instance Generic S80 where
  type Code S80 = S80Code

  from :: S80 -> SOP I S80Code
  from S80_00 = SOP (unK (apFn inj00 Nil))
  from S80_01 = SOP (unK (apFn inj01 Nil))
  from S80_02 = SOP (unK (apFn inj02 Nil))
  from S80_03 = SOP (unK (apFn inj03 Nil))
  from S80_04 = SOP (unK (apFn inj04 Nil))
  from S80_05 = SOP (unK (apFn inj05 Nil))
  from S80_06 = SOP (unK (apFn inj06 Nil))
  from S80_07 = SOP (unK (apFn inj07 Nil))
  from S80_08 = SOP (unK (apFn inj08 Nil))
  from S80_09 = SOP (unK (apFn inj09 Nil))
  from S80_10 = SOP (unK (apFn inj10 Nil))
  from S80_11 = SOP (unK (apFn inj11 Nil))
  from S80_12 = SOP (unK (apFn inj12 Nil))
  from S80_13 = SOP (unK (apFn inj13 Nil))
  from S80_14 = SOP (unK (apFn inj14 Nil))
  from S80_15 = SOP (unK (apFn inj15 Nil))
  from S80_16 = SOP (unK (apFn inj16 Nil))
  from S80_17 = SOP (unK (apFn inj17 Nil))
  from S80_18 = SOP (unK (apFn inj18 Nil))
  from S80_19 = SOP (unK (apFn inj19 Nil))
  from S80_20 = SOP (unK (apFn inj20 Nil))
  from S80_21 = SOP (unK (apFn inj21 Nil))
  from S80_22 = SOP (unK (apFn inj22 Nil))
  from S80_23 = SOP (unK (apFn inj23 Nil))
  from S80_24 = SOP (unK (apFn inj24 Nil))
  from S80_25 = SOP (unK (apFn inj25 Nil))
  from S80_26 = SOP (unK (apFn inj26 Nil))
  from S80_27 = SOP (unK (apFn inj27 Nil))
  from S80_28 = SOP (unK (apFn inj28 Nil))
  from S80_29 = SOP (unK (apFn inj29 Nil))
  from S80_30 = SOP (unK (apFn inj30 Nil))
  from S80_31 = SOP (unK (apFn inj31 Nil))
  from S80_32 = SOP (unK (apFn inj32 Nil))
  from S80_33 = SOP (unK (apFn inj33 Nil))
  from S80_34 = SOP (unK (apFn inj34 Nil))
  from S80_35 = SOP (unK (apFn inj35 Nil))
  from S80_36 = SOP (unK (apFn inj36 Nil))
  from S80_37 = SOP (unK (apFn inj37 Nil))
  from S80_38 = SOP (unK (apFn inj38 Nil))
  from S80_39 = SOP (unK (apFn inj39 Nil))
  from S80_40 = SOP (unK (apFn inj40 Nil))
  from S80_41 = SOP (unK (apFn inj41 Nil))
  from S80_42 = SOP (unK (apFn inj42 Nil))
  from S80_43 = SOP (unK (apFn inj43 Nil))
  from S80_44 = SOP (unK (apFn inj44 Nil))
  from S80_45 = SOP (unK (apFn inj45 Nil))
  from S80_46 = SOP (unK (apFn inj46 Nil))
  from S80_47 = SOP (unK (apFn inj47 Nil))
  from S80_48 = SOP (unK (apFn inj48 Nil))
  from S80_49 = SOP (unK (apFn inj49 Nil))
  from S80_50 = SOP (unK (apFn inj50 Nil))
  from S80_51 = SOP (unK (apFn inj51 Nil))
  from S80_52 = SOP (unK (apFn inj52 Nil))
  from S80_53 = SOP (unK (apFn inj53 Nil))
  from S80_54 = SOP (unK (apFn inj54 Nil))
  from S80_55 = SOP (unK (apFn inj55 Nil))
  from S80_56 = SOP (unK (apFn inj56 Nil))
  from S80_57 = SOP (unK (apFn inj57 Nil))
  from S80_58 = SOP (unK (apFn inj58 Nil))
  from S80_59 = SOP (unK (apFn inj59 Nil))
  from S80_60 = SOP (unK (apFn inj60 Nil))
  from S80_61 = SOP (unK (apFn inj61 Nil))
  from S80_62 = SOP (unK (apFn inj62 Nil))
  from S80_63 = SOP (unK (apFn inj63 Nil))
  from S80_64 = SOP (unK (apFn inj64 Nil))
  from S80_65 = SOP (unK (apFn inj65 Nil))
  from S80_66 = SOP (unK (apFn inj66 Nil))
  from S80_67 = SOP (unK (apFn inj67 Nil))
  from S80_68 = SOP (unK (apFn inj68 Nil))
  from S80_69 = SOP (unK (apFn inj69 Nil))
  from S80_70 = SOP (unK (apFn inj70 Nil))
  from S80_71 = SOP (unK (apFn inj71 Nil))
  from S80_72 = SOP (unK (apFn inj72 Nil))
  from S80_73 = SOP (unK (apFn inj73 Nil))
  from S80_74 = SOP (unK (apFn inj74 Nil))
  from S80_75 = SOP (unK (apFn inj75 Nil))
  from S80_76 = SOP (unK (apFn inj76 Nil))
  from S80_77 = SOP (unK (apFn inj77 Nil))
  from S80_78 = SOP (unK (apFn inj78 Nil))
  from S80_79 = SOP (unK (apFn inj79 Nil))

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
