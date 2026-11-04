{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
-- | As "CoreSize.S10.THNoSig", at 20 constructors, to compare against
-- "CoreSize.S20.TH".
module CoreSize.S20.THNoSig where

import Data.Kind (Type)
import Generics.SOP
import qualified Generics.SOP.Type.Metadata as T

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

type S20Code19 = '[ '[]] :: [[Type]]
type S20Code18 =
    '(:) '[] S20Code19 :: [[Type]]
type S20Code17 =
    '(:) '[] S20Code18 :: [[Type]]
type S20Code16 =
    '(:) '[] S20Code17 :: [[Type]]
type S20Code15 =
    '(:) '[] S20Code16 :: [[Type]]
type S20Code14 =
    '(:) '[] S20Code15 :: [[Type]]
type S20Code13 =
    '(:) '[] S20Code14 :: [[Type]]
type S20Code12 =
    '(:) '[] S20Code13 :: [[Type]]
type S20Code11 =
    '(:) '[] S20Code12 :: [[Type]]
type S20Code10 =
    '(:) '[] S20Code11 :: [[Type]]
type S20Code9 =
    '(:) '[] S20Code10 :: [[Type]]
type S20Code8 =
    '(:) '[] S20Code9 :: [[Type]]
type S20Code7 =
    '(:) '[] S20Code8 :: [[Type]]
type S20Code6 =
    '(:) '[] S20Code7 :: [[Type]]
type S20Code5 =
    '(:) '[] S20Code6 :: [[Type]]
type S20Code4 =
    '(:) '[] S20Code5 :: [[Type]]
type S20Code3 =
    '(:) '[] S20Code4 :: [[Type]]
type S20Code2 =
    '(:) '[] S20Code3 :: [[Type]]
type S20Code1 =
    '(:) '[] S20Code2 :: [[Type]]
type S20Code =
    '(:) '[] S20Code1 :: [[Type]]
instance Generic S20 where
  type Code S20 = S20Code
  from S20_00
    = SOP (Z Nil)
  from S20_01
    = SOP (S (Z Nil))
  from S20_02
    = SOP
        (S (S (Z Nil)))
  from S20_03
    = SOP
        (S
           (S (S (Z Nil))))
  from S20_04
    = SOP
        (S
           (S
              (S (S (Z Nil)))))
  from S20_05
    = SOP
        (S
           (S
              (S
                 (S (S (Z Nil))))))
  from S20_06
    = SOP
        (S
           (S
              (S
                 (S
                    (S (S (Z Nil)))))))
  from S20_07
    = SOP
        (S
           (S
              (S
                 (S
                    (S
                       (S
                          (S (Z Nil))))))))
  from S20_08
    = SOP
        (S
           (S
              (S
                 (S
                    (S
                       (S
                          (S
                             (S (Z Nil)))))))))
  from S20_09
    = SOP
        (S
           (S
              (S
                 (S
                    (S
                       (S
                          (S
                             (S
                                (S (Z Nil))))))))))
  from S20_10
    = SOP
        (S
           (S
              (S
                 (S
                    (S
                       (S
                          (S
                             (S
                                (S
                                   (S (Z Nil)))))))))))
  from S20_11
    = SOP
        (S
           (S
              (S
                 (S
                    (S
                       (S
                          (S
                             (S
                                (S
                                   (S
                                      (S (Z Nil))))))))))))
  from S20_12
    = SOP
        (S
           (S
              (S
                 (S
                    (S
                       (S
                          (S
                             (S
                                (S
                                   (S
                                      (S
                                         (S
                                            (Z Nil)))))))))))))
  from S20_13
    = SOP
        (S
           (S
              (S
                 (S
                    (S
                       (S
                          (S
                             (S
                                (S
                                   (S
                                      (S
                                         (S
                                            (S
                                               (Z Nil))))))))))))))
  from S20_14
    = SOP
        (S
           (S
              (S
                 (S
                    (S
                       (S
                          (S
                             (S
                                (S
                                   (S
                                      (S
                                         (S
                                            (S
                                               (S
                                                  (Z Nil)))))))))))))))
  from S20_15
    = SOP
        (S
           (S
              (S
                 (S
                    (S
                       (S
                          (S
                             (S
                                (S
                                   (S
                                      (S
                                         (S
                                            (S
                                               (S
                                                  (S
                                                     (Z
                                                        Nil))))))))))))))))
  from S20_16
    = SOP
        (S
           (S
              (S
                 (S
                    (S
                       (S
                          (S
                             (S
                                (S
                                   (S
                                      (S
                                         (S
                                            (S
                                               (S
                                                  (S
                                                     (S
                                                        (Z
                                                           Nil)))))))))))))))))
  from S20_17
    = SOP
        (S
           (S
              (S
                 (S
                    (S
                       (S
                          (S
                             (S
                                (S
                                   (S
                                      (S
                                         (S
                                            (S
                                               (S
                                                  (S
                                                     (S
                                                        (S
                                                           (Z
                                                              Nil))))))))))))))))))
  from S20_18
    = SOP
        (S
           (S
              (S
                 (S
                    (S
                       (S
                          (S
                             (S
                                (S
                                   (S
                                      (S
                                         (S
                                            (S
                                               (S
                                                  (S
                                                     (S
                                                        (S
                                                           (S
                                                              (Z
                                                                 Nil)))))))))))))))))))
  from S20_19
    = SOP
        (S
           (S
              (S
                 (S
                    (S
                       (S
                          (S
                             (S
                                (S
                                   (S
                                      (S
                                         (S
                                            (S
                                               (S
                                                  (S
                                                     (S
                                                        (S
                                                           (S
                                                              (S
                                                                 (Z
                                                                    Nil))))))))))))))))))))
  to
    (SOP (Z Nil))
    = S20_00
  to
    (SOP (S (Z Nil)))
    = S20_01
  to
    (SOP (S (S (Z Nil))))
    = S20_02
  to
    (SOP (S (S (S (Z Nil)))))
    = S20_03
  to
    (SOP (S (S (S (S (Z Nil))))))
    = S20_04
  to
    (SOP (S (S (S (S (S (Z Nil)))))))
    = S20_05
  to
    (SOP (S (S (S (S (S (S (Z Nil))))))))
    = S20_06
  to
    (SOP (S (S (S (S (S (S (S (Z Nil)))))))))
    = S20_07
  to
    (SOP (S (S (S (S (S (S (S (S (Z Nil))))))))))
    = S20_08
  to
    (SOP (S (S (S (S (S (S (S (S (S (Z Nil)))))))))))
    = S20_09
  to
    (SOP (S (S (S (S (S (S (S (S (S (S (Z Nil))))))))))))
    = S20_10
  to
    (SOP (S (S (S (S (S (S (S (S (S (S (S (Z Nil)))))))))))))
    = S20_11
  to
    (SOP (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil))))))))))))))
    = S20_12
  to
    (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil)))))))))))))))
    = S20_13
  to
    (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil))))))))))))))))
    = S20_14
  to
    (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil)))))))))))))))))
    = S20_15
  to
    (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil))))))))))))))))))
    = S20_16
  to
    (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil)))))))))))))))))))
    = S20_17
  to
    (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil))))))))))))))))))))
    = S20_18
  to
    (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil)))))))))))))))))))))
    = S20_19
  to
    (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S x)))))))))))))))))))))
    = (x `seq` error "inaccessible")
type S20ConstructorS20_00 =
    'T.Constructor "S20_00" :: T.ConstructorInfo
type S20ConstructorS20_01 =
    'T.Constructor "S20_01" :: T.ConstructorInfo
type S20ConstructorS20_02 =
    'T.Constructor "S20_02" :: T.ConstructorInfo
type S20ConstructorS20_03 =
    'T.Constructor "S20_03" :: T.ConstructorInfo
type S20ConstructorS20_04 =
    'T.Constructor "S20_04" :: T.ConstructorInfo
type S20ConstructorS20_05 =
    'T.Constructor "S20_05" :: T.ConstructorInfo
type S20ConstructorS20_06 =
    'T.Constructor "S20_06" :: T.ConstructorInfo
type S20ConstructorS20_07 =
    'T.Constructor "S20_07" :: T.ConstructorInfo
type S20ConstructorS20_08 =
    'T.Constructor "S20_08" :: T.ConstructorInfo
type S20ConstructorS20_09 =
    'T.Constructor "S20_09" :: T.ConstructorInfo
type S20ConstructorS20_10 =
    'T.Constructor "S20_10" :: T.ConstructorInfo
type S20ConstructorS20_11 =
    'T.Constructor "S20_11" :: T.ConstructorInfo
type S20ConstructorS20_12 =
    'T.Constructor "S20_12" :: T.ConstructorInfo
type S20ConstructorS20_13 =
    'T.Constructor "S20_13" :: T.ConstructorInfo
type S20ConstructorS20_14 =
    'T.Constructor "S20_14" :: T.ConstructorInfo
type S20ConstructorS20_15 =
    'T.Constructor "S20_15" :: T.ConstructorInfo
type S20ConstructorS20_16 =
    'T.Constructor "S20_16" :: T.ConstructorInfo
type S20ConstructorS20_17 =
    'T.Constructor "S20_17" :: T.ConstructorInfo
type S20ConstructorS20_18 =
    'T.Constructor "S20_18" :: T.ConstructorInfo
type S20ConstructorS20_19 =
    'T.Constructor "S20_19" :: T.ConstructorInfo
type S20Constructors19 =
    '[S20ConstructorS20_19] :: [T.ConstructorInfo]
type S20Constructors18 =
    '(:) S20ConstructorS20_18 S20Constructors19 :: [T.ConstructorInfo]
type S20Constructors17 =
    '(:) S20ConstructorS20_17 S20Constructors18 :: [T.ConstructorInfo]
type S20Constructors16 =
    '(:) S20ConstructorS20_16 S20Constructors17 :: [T.ConstructorInfo]
type S20Constructors15 =
    '(:) S20ConstructorS20_15 S20Constructors16 :: [T.ConstructorInfo]
type S20Constructors14 =
    '(:) S20ConstructorS20_14 S20Constructors15 :: [T.ConstructorInfo]
type S20Constructors13 =
    '(:) S20ConstructorS20_13 S20Constructors14 :: [T.ConstructorInfo]
type S20Constructors12 =
    '(:) S20ConstructorS20_12 S20Constructors13 :: [T.ConstructorInfo]
type S20Constructors11 =
    '(:) S20ConstructorS20_11 S20Constructors12 :: [T.ConstructorInfo]
type S20Constructors10 =
    '(:) S20ConstructorS20_10 S20Constructors11 :: [T.ConstructorInfo]
type S20Constructors9 =
    '(:) S20ConstructorS20_09 S20Constructors10 :: [T.ConstructorInfo]
type S20Constructors8 =
    '(:) S20ConstructorS20_08 S20Constructors9 :: [T.ConstructorInfo]
type S20Constructors7 =
    '(:) S20ConstructorS20_07 S20Constructors8 :: [T.ConstructorInfo]
type S20Constructors6 =
    '(:) S20ConstructorS20_06 S20Constructors7 :: [T.ConstructorInfo]
type S20Constructors5 =
    '(:) S20ConstructorS20_05 S20Constructors6 :: [T.ConstructorInfo]
type S20Constructors4 =
    '(:) S20ConstructorS20_04 S20Constructors5 :: [T.ConstructorInfo]
type S20Constructors3 =
    '(:) S20ConstructorS20_03 S20Constructors4 :: [T.ConstructorInfo]
type S20Constructors2 =
    '(:) S20ConstructorS20_02 S20Constructors3 :: [T.ConstructorInfo]
type S20Constructors1 =
    '(:) S20ConstructorS20_01 S20Constructors2 :: [T.ConstructorInfo]
type S20Constructors =
    '(:) S20ConstructorS20_00 S20Constructors1 :: [T.ConstructorInfo]
type S20Strictnesses19 =
    '[ '[]] :: [[T.StrictnessInfo]]
type S20Strictnesses18 =
    '(:) '[] S20Strictnesses19 :: [[T.StrictnessInfo]]
type S20Strictnesses17 =
    '(:) '[] S20Strictnesses18 :: [[T.StrictnessInfo]]
type S20Strictnesses16 =
    '(:) '[] S20Strictnesses17 :: [[T.StrictnessInfo]]
type S20Strictnesses15 =
    '(:) '[] S20Strictnesses16 :: [[T.StrictnessInfo]]
type S20Strictnesses14 =
    '(:) '[] S20Strictnesses15 :: [[T.StrictnessInfo]]
type S20Strictnesses13 =
    '(:) '[] S20Strictnesses14 :: [[T.StrictnessInfo]]
type S20Strictnesses12 =
    '(:) '[] S20Strictnesses13 :: [[T.StrictnessInfo]]
type S20Strictnesses11 =
    '(:) '[] S20Strictnesses12 :: [[T.StrictnessInfo]]
type S20Strictnesses10 =
    '(:) '[] S20Strictnesses11 :: [[T.StrictnessInfo]]
type S20Strictnesses9 =
    '(:) '[] S20Strictnesses10 :: [[T.StrictnessInfo]]
type S20Strictnesses8 =
    '(:) '[] S20Strictnesses9 :: [[T.StrictnessInfo]]
type S20Strictnesses7 =
    '(:) '[] S20Strictnesses8 :: [[T.StrictnessInfo]]
type S20Strictnesses6 =
    '(:) '[] S20Strictnesses7 :: [[T.StrictnessInfo]]
type S20Strictnesses5 =
    '(:) '[] S20Strictnesses6 :: [[T.StrictnessInfo]]
type S20Strictnesses4 =
    '(:) '[] S20Strictnesses5 :: [[T.StrictnessInfo]]
type S20Strictnesses3 =
    '(:) '[] S20Strictnesses4 :: [[T.StrictnessInfo]]
type S20Strictnesses2 =
    '(:) '[] S20Strictnesses3 :: [[T.StrictnessInfo]]
type S20Strictnesses1 =
    '(:) '[] S20Strictnesses2 :: [[T.StrictnessInfo]]
type S20Strictnesses =
    '(:) '[] S20Strictnesses1 :: [[T.StrictnessInfo]]
instance HasDatatypeInfo S20 where
  type DatatypeInfoOf S20 = 'T.ADT "CoreSize.S20.THNoSig" "S20" S20Constructors S20Strictnesses
  datatypeInfo _
    = T.demoteDatatypeInfo
        (Proxy ::
           Proxy (DatatypeInfoOf S20))
