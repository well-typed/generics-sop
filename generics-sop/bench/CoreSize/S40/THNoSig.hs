{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
-- | As "CoreSize.S10.THNoSig", at 40 constructors, to compare against
-- "CoreSize.S40.TH".
module CoreSize.S40.THNoSig where

import Data.Kind (Type)
import Generics.SOP
import qualified Generics.SOP.Type.Metadata as T

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

type S40Code39 = '[ '[]] :: [[Type]]
type S40Code38 =
    '(:) '[] S40Code39 :: [[Type]]
type S40Code37 =
    '(:) '[] S40Code38 :: [[Type]]
type S40Code36 =
    '(:) '[] S40Code37 :: [[Type]]
type S40Code35 =
    '(:) '[] S40Code36 :: [[Type]]
type S40Code34 =
    '(:) '[] S40Code35 :: [[Type]]
type S40Code33 =
    '(:) '[] S40Code34 :: [[Type]]
type S40Code32 =
    '(:) '[] S40Code33 :: [[Type]]
type S40Code31 =
    '(:) '[] S40Code32 :: [[Type]]
type S40Code30 =
    '(:) '[] S40Code31 :: [[Type]]
type S40Code29 =
    '(:) '[] S40Code30 :: [[Type]]
type S40Code28 =
    '(:) '[] S40Code29 :: [[Type]]
type S40Code27 =
    '(:) '[] S40Code28 :: [[Type]]
type S40Code26 =
    '(:) '[] S40Code27 :: [[Type]]
type S40Code25 =
    '(:) '[] S40Code26 :: [[Type]]
type S40Code24 =
    '(:) '[] S40Code25 :: [[Type]]
type S40Code23 =
    '(:) '[] S40Code24 :: [[Type]]
type S40Code22 =
    '(:) '[] S40Code23 :: [[Type]]
type S40Code21 =
    '(:) '[] S40Code22 :: [[Type]]
type S40Code20 =
    '(:) '[] S40Code21 :: [[Type]]
type S40Code19 =
    '(:) '[] S40Code20 :: [[Type]]
type S40Code18 =
    '(:) '[] S40Code19 :: [[Type]]
type S40Code17 =
    '(:) '[] S40Code18 :: [[Type]]
type S40Code16 =
    '(:) '[] S40Code17 :: [[Type]]
type S40Code15 =
    '(:) '[] S40Code16 :: [[Type]]
type S40Code14 =
    '(:) '[] S40Code15 :: [[Type]]
type S40Code13 =
    '(:) '[] S40Code14 :: [[Type]]
type S40Code12 =
    '(:) '[] S40Code13 :: [[Type]]
type S40Code11 =
    '(:) '[] S40Code12 :: [[Type]]
type S40Code10 =
    '(:) '[] S40Code11 :: [[Type]]
type S40Code9 =
    '(:) '[] S40Code10 :: [[Type]]
type S40Code8 =
    '(:) '[] S40Code9 :: [[Type]]
type S40Code7 =
    '(:) '[] S40Code8 :: [[Type]]
type S40Code6 =
    '(:) '[] S40Code7 :: [[Type]]
type S40Code5 =
    '(:) '[] S40Code6 :: [[Type]]
type S40Code4 =
    '(:) '[] S40Code5 :: [[Type]]
type S40Code3 =
    '(:) '[] S40Code4 :: [[Type]]
type S40Code2 =
    '(:) '[] S40Code3 :: [[Type]]
type S40Code1 =
    '(:) '[] S40Code2 :: [[Type]]
type S40Code =
    '(:) '[] S40Code1 :: [[Type]]
instance Generic S40 where
  type Code S40 = S40Code
  from S40_00
    = SOP (Z Nil)
  from S40_01
    = SOP (S (Z Nil))
  from S40_02
    = SOP
        (S (S (Z Nil)))
  from S40_03
    = SOP
        (S
           (S (S (Z Nil))))
  from S40_04
    = SOP
        (S
           (S
              (S (S (Z Nil)))))
  from S40_05
    = SOP
        (S
           (S
              (S
                 (S (S (Z Nil))))))
  from S40_06
    = SOP
        (S
           (S
              (S
                 (S
                    (S (S (Z Nil)))))))
  from S40_07
    = SOP
        (S
           (S
              (S
                 (S
                    (S
                       (S
                          (S (Z Nil))))))))
  from S40_08
    = SOP
        (S
           (S
              (S
                 (S
                    (S
                       (S
                          (S
                             (S (Z Nil)))))))))
  from S40_09
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
  from S40_10
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
  from S40_11
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
  from S40_12
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
  from S40_13
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
  from S40_14
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
  from S40_15
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
  from S40_16
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
  from S40_17
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
  from S40_18
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
  from S40_19
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
  from S40_20
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
                                                                 (S
                                                                    (Z
                                                                       Nil)))))))))))))))))))))
  from S40_21
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
                                                                 (S
                                                                    (S
                                                                       (Z
                                                                          Nil))))))))))))))))))))))
  from S40_22
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
                                                                 (S
                                                                    (S
                                                                       (S
                                                                          (Z
                                                                             Nil)))))))))))))))))))))))
  from S40_23
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
                                                                 (S
                                                                    (S
                                                                       (S
                                                                          (S
                                                                             (Z
                                                                                Nil))))))))))))))))))))))))
  from S40_24
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
                                                                 (S
                                                                    (S
                                                                       (S
                                                                          (S
                                                                             (S
                                                                                (Z
                                                                                   Nil)))))))))))))))))))))))))
  from S40_25
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
                                                                 (S
                                                                    (S
                                                                       (S
                                                                          (S
                                                                             (S
                                                                                (S
                                                                                   (Z
                                                                                      Nil))))))))))))))))))))))))))
  from S40_26
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
                                                                 (S
                                                                    (S
                                                                       (S
                                                                          (S
                                                                             (S
                                                                                (S
                                                                                   (S
                                                                                      (Z
                                                                                         Nil)))))))))))))))))))))))))))
  from S40_27
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
                                                                 (S
                                                                    (S
                                                                       (S
                                                                          (S
                                                                             (S
                                                                                (S
                                                                                   (S
                                                                                      (S
                                                                                         (Z
                                                                                            Nil))))))))))))))))))))))))))))
  from S40_28
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
                                                                                               Nil)))))))))))))))))))))))))))))
  from S40_29
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
                                                                                                  Nil))))))))))))))))))))))))))))))
  from S40_30
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
                                                                                                     Nil)))))))))))))))))))))))))))))))
  from S40_31
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
                                                                                                        Nil))))))))))))))))))))))))))))))))
  from S40_32
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
                                                                                                           Nil)))))))))))))))))))))))))))))))))
  from S40_33
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
                                                                                                              Nil))))))))))))))))))))))))))))))))))
  from S40_34
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
                                                                                                                 Nil)))))))))))))))))))))))))))))))))))
  from S40_35
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
                                                                                                                    Nil))))))))))))))))))))))))))))))))))))
  from S40_36
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
                                                                                                                       Nil)))))))))))))))))))))))))))))))))))))
  from S40_37
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
                                                                                                                          Nil))))))))))))))))))))))))))))))))))))))
  from S40_38
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
                                                                                                                             Nil)))))))))))))))))))))))))))))))))))))))
  from S40_39
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
                                                                                                                          (S
                                                                                                                             (Z
                                                                                                                                Nil))))))))))))))))))))))))))))))))))))))))
  to
    (SOP (Z Nil))
    = S40_00
  to
    (SOP (S (Z Nil)))
    = S40_01
  to
    (SOP (S (S (Z Nil))))
    = S40_02
  to
    (SOP (S (S (S (Z Nil)))))
    = S40_03
  to
    (SOP (S (S (S (S (Z Nil))))))
    = S40_04
  to
    (SOP (S (S (S (S (S (Z Nil)))))))
    = S40_05
  to
    (SOP (S (S (S (S (S (S (Z Nil))))))))
    = S40_06
  to
    (SOP (S (S (S (S (S (S (S (Z Nil)))))))))
    = S40_07
  to
    (SOP (S (S (S (S (S (S (S (S (Z Nil))))))))))
    = S40_08
  to
    (SOP (S (S (S (S (S (S (S (S (S (Z Nil)))))))))))
    = S40_09
  to
    (SOP (S (S (S (S (S (S (S (S (S (S (Z Nil))))))))))))
    = S40_10
  to
    (SOP (S (S (S (S (S (S (S (S (S (S (S (Z Nil)))))))))))))
    = S40_11
  to
    (SOP (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil))))))))))))))
    = S40_12
  to
    (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil)))))))))))))))
    = S40_13
  to
    (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil))))))))))))))))
    = S40_14
  to
    (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil)))))))))))))))))
    = S40_15
  to
    (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil))))))))))))))))))
    = S40_16
  to
    (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil)))))))))))))))))))
    = S40_17
  to
    (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil))))))))))))))))))))
    = S40_18
  to
    (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil)))))))))))))))))))))
    = S40_19
  to
    (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil))))))))))))))))))))))
    = S40_20
  to
    (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil)))))))))))))))))))))))
    = S40_21
  to
    (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil))))))))))))))))))))))))
    = S40_22
  to
    (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil)))))))))))))))))))))))))
    = S40_23
  to
    (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil))))))))))))))))))))))))))
    = S40_24
  to
    (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil)))))))))))))))))))))))))))
    = S40_25
  to
    (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil))))))))))))))))))))))))))))
    = S40_26
  to
    (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil)))))))))))))))))))))))))))))
    = S40_27
  to
    (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil))))))))))))))))))))))))))))))
    = S40_28
  to
    (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil)))))))))))))))))))))))))))))))
    = S40_29
  to
    (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil))))))))))))))))))))))))))))))))
    = S40_30
  to
    (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil)))))))))))))))))))))))))))))))))
    = S40_31
  to
    (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil))))))))))))))))))))))))))))))))))
    = S40_32
  to
    (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil)))))))))))))))))))))))))))))))))))
    = S40_33
  to
    (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil))))))))))))))))))))))))))))))))))))
    = S40_34
  to
    (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil)))))))))))))))))))))))))))))))))))))
    = S40_35
  to
    (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil))))))))))))))))))))))))))))))))))))))
    = S40_36
  to
    (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil)))))))))))))))))))))))))))))))))))))))
    = S40_37
  to
    (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil))))))))))))))))))))))))))))))))))))))))
    = S40_38
  to
    (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil)))))))))))))))))))))))))))))))))))))))))
    = S40_39
  to
    (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S x)))))))))))))))))))))))))))))))))))))))))
    = (x `seq` error "inaccessible")
type S40ConstructorS40_00 =
    'T.Constructor "S40_00" :: T.ConstructorInfo
type S40ConstructorS40_01 =
    'T.Constructor "S40_01" :: T.ConstructorInfo
type S40ConstructorS40_02 =
    'T.Constructor "S40_02" :: T.ConstructorInfo
type S40ConstructorS40_03 =
    'T.Constructor "S40_03" :: T.ConstructorInfo
type S40ConstructorS40_04 =
    'T.Constructor "S40_04" :: T.ConstructorInfo
type S40ConstructorS40_05 =
    'T.Constructor "S40_05" :: T.ConstructorInfo
type S40ConstructorS40_06 =
    'T.Constructor "S40_06" :: T.ConstructorInfo
type S40ConstructorS40_07 =
    'T.Constructor "S40_07" :: T.ConstructorInfo
type S40ConstructorS40_08 =
    'T.Constructor "S40_08" :: T.ConstructorInfo
type S40ConstructorS40_09 =
    'T.Constructor "S40_09" :: T.ConstructorInfo
type S40ConstructorS40_10 =
    'T.Constructor "S40_10" :: T.ConstructorInfo
type S40ConstructorS40_11 =
    'T.Constructor "S40_11" :: T.ConstructorInfo
type S40ConstructorS40_12 =
    'T.Constructor "S40_12" :: T.ConstructorInfo
type S40ConstructorS40_13 =
    'T.Constructor "S40_13" :: T.ConstructorInfo
type S40ConstructorS40_14 =
    'T.Constructor "S40_14" :: T.ConstructorInfo
type S40ConstructorS40_15 =
    'T.Constructor "S40_15" :: T.ConstructorInfo
type S40ConstructorS40_16 =
    'T.Constructor "S40_16" :: T.ConstructorInfo
type S40ConstructorS40_17 =
    'T.Constructor "S40_17" :: T.ConstructorInfo
type S40ConstructorS40_18 =
    'T.Constructor "S40_18" :: T.ConstructorInfo
type S40ConstructorS40_19 =
    'T.Constructor "S40_19" :: T.ConstructorInfo
type S40ConstructorS40_20 =
    'T.Constructor "S40_20" :: T.ConstructorInfo
type S40ConstructorS40_21 =
    'T.Constructor "S40_21" :: T.ConstructorInfo
type S40ConstructorS40_22 =
    'T.Constructor "S40_22" :: T.ConstructorInfo
type S40ConstructorS40_23 =
    'T.Constructor "S40_23" :: T.ConstructorInfo
type S40ConstructorS40_24 =
    'T.Constructor "S40_24" :: T.ConstructorInfo
type S40ConstructorS40_25 =
    'T.Constructor "S40_25" :: T.ConstructorInfo
type S40ConstructorS40_26 =
    'T.Constructor "S40_26" :: T.ConstructorInfo
type S40ConstructorS40_27 =
    'T.Constructor "S40_27" :: T.ConstructorInfo
type S40ConstructorS40_28 =
    'T.Constructor "S40_28" :: T.ConstructorInfo
type S40ConstructorS40_29 =
    'T.Constructor "S40_29" :: T.ConstructorInfo
type S40ConstructorS40_30 =
    'T.Constructor "S40_30" :: T.ConstructorInfo
type S40ConstructorS40_31 =
    'T.Constructor "S40_31" :: T.ConstructorInfo
type S40ConstructorS40_32 =
    'T.Constructor "S40_32" :: T.ConstructorInfo
type S40ConstructorS40_33 =
    'T.Constructor "S40_33" :: T.ConstructorInfo
type S40ConstructorS40_34 =
    'T.Constructor "S40_34" :: T.ConstructorInfo
type S40ConstructorS40_35 =
    'T.Constructor "S40_35" :: T.ConstructorInfo
type S40ConstructorS40_36 =
    'T.Constructor "S40_36" :: T.ConstructorInfo
type S40ConstructorS40_37 =
    'T.Constructor "S40_37" :: T.ConstructorInfo
type S40ConstructorS40_38 =
    'T.Constructor "S40_38" :: T.ConstructorInfo
type S40ConstructorS40_39 =
    'T.Constructor "S40_39" :: T.ConstructorInfo
type S40Constructors39 =
    '[S40ConstructorS40_39] :: [T.ConstructorInfo]
type S40Constructors38 =
    '(:) S40ConstructorS40_38 S40Constructors39 :: [T.ConstructorInfo]
type S40Constructors37 =
    '(:) S40ConstructorS40_37 S40Constructors38 :: [T.ConstructorInfo]
type S40Constructors36 =
    '(:) S40ConstructorS40_36 S40Constructors37 :: [T.ConstructorInfo]
type S40Constructors35 =
    '(:) S40ConstructorS40_35 S40Constructors36 :: [T.ConstructorInfo]
type S40Constructors34 =
    '(:) S40ConstructorS40_34 S40Constructors35 :: [T.ConstructorInfo]
type S40Constructors33 =
    '(:) S40ConstructorS40_33 S40Constructors34 :: [T.ConstructorInfo]
type S40Constructors32 =
    '(:) S40ConstructorS40_32 S40Constructors33 :: [T.ConstructorInfo]
type S40Constructors31 =
    '(:) S40ConstructorS40_31 S40Constructors32 :: [T.ConstructorInfo]
type S40Constructors30 =
    '(:) S40ConstructorS40_30 S40Constructors31 :: [T.ConstructorInfo]
type S40Constructors29 =
    '(:) S40ConstructorS40_29 S40Constructors30 :: [T.ConstructorInfo]
type S40Constructors28 =
    '(:) S40ConstructorS40_28 S40Constructors29 :: [T.ConstructorInfo]
type S40Constructors27 =
    '(:) S40ConstructorS40_27 S40Constructors28 :: [T.ConstructorInfo]
type S40Constructors26 =
    '(:) S40ConstructorS40_26 S40Constructors27 :: [T.ConstructorInfo]
type S40Constructors25 =
    '(:) S40ConstructorS40_25 S40Constructors26 :: [T.ConstructorInfo]
type S40Constructors24 =
    '(:) S40ConstructorS40_24 S40Constructors25 :: [T.ConstructorInfo]
type S40Constructors23 =
    '(:) S40ConstructorS40_23 S40Constructors24 :: [T.ConstructorInfo]
type S40Constructors22 =
    '(:) S40ConstructorS40_22 S40Constructors23 :: [T.ConstructorInfo]
type S40Constructors21 =
    '(:) S40ConstructorS40_21 S40Constructors22 :: [T.ConstructorInfo]
type S40Constructors20 =
    '(:) S40ConstructorS40_20 S40Constructors21 :: [T.ConstructorInfo]
type S40Constructors19 =
    '(:) S40ConstructorS40_19 S40Constructors20 :: [T.ConstructorInfo]
type S40Constructors18 =
    '(:) S40ConstructorS40_18 S40Constructors19 :: [T.ConstructorInfo]
type S40Constructors17 =
    '(:) S40ConstructorS40_17 S40Constructors18 :: [T.ConstructorInfo]
type S40Constructors16 =
    '(:) S40ConstructorS40_16 S40Constructors17 :: [T.ConstructorInfo]
type S40Constructors15 =
    '(:) S40ConstructorS40_15 S40Constructors16 :: [T.ConstructorInfo]
type S40Constructors14 =
    '(:) S40ConstructorS40_14 S40Constructors15 :: [T.ConstructorInfo]
type S40Constructors13 =
    '(:) S40ConstructorS40_13 S40Constructors14 :: [T.ConstructorInfo]
type S40Constructors12 =
    '(:) S40ConstructorS40_12 S40Constructors13 :: [T.ConstructorInfo]
type S40Constructors11 =
    '(:) S40ConstructorS40_11 S40Constructors12 :: [T.ConstructorInfo]
type S40Constructors10 =
    '(:) S40ConstructorS40_10 S40Constructors11 :: [T.ConstructorInfo]
type S40Constructors9 =
    '(:) S40ConstructorS40_09 S40Constructors10 :: [T.ConstructorInfo]
type S40Constructors8 =
    '(:) S40ConstructorS40_08 S40Constructors9 :: [T.ConstructorInfo]
type S40Constructors7 =
    '(:) S40ConstructorS40_07 S40Constructors8 :: [T.ConstructorInfo]
type S40Constructors6 =
    '(:) S40ConstructorS40_06 S40Constructors7 :: [T.ConstructorInfo]
type S40Constructors5 =
    '(:) S40ConstructorS40_05 S40Constructors6 :: [T.ConstructorInfo]
type S40Constructors4 =
    '(:) S40ConstructorS40_04 S40Constructors5 :: [T.ConstructorInfo]
type S40Constructors3 =
    '(:) S40ConstructorS40_03 S40Constructors4 :: [T.ConstructorInfo]
type S40Constructors2 =
    '(:) S40ConstructorS40_02 S40Constructors3 :: [T.ConstructorInfo]
type S40Constructors1 =
    '(:) S40ConstructorS40_01 S40Constructors2 :: [T.ConstructorInfo]
type S40Constructors =
    '(:) S40ConstructorS40_00 S40Constructors1 :: [T.ConstructorInfo]
type S40Strictnesses39 =
    '[ '[]] :: [[T.StrictnessInfo]]
type S40Strictnesses38 =
    '(:) '[] S40Strictnesses39 :: [[T.StrictnessInfo]]
type S40Strictnesses37 =
    '(:) '[] S40Strictnesses38 :: [[T.StrictnessInfo]]
type S40Strictnesses36 =
    '(:) '[] S40Strictnesses37 :: [[T.StrictnessInfo]]
type S40Strictnesses35 =
    '(:) '[] S40Strictnesses36 :: [[T.StrictnessInfo]]
type S40Strictnesses34 =
    '(:) '[] S40Strictnesses35 :: [[T.StrictnessInfo]]
type S40Strictnesses33 =
    '(:) '[] S40Strictnesses34 :: [[T.StrictnessInfo]]
type S40Strictnesses32 =
    '(:) '[] S40Strictnesses33 :: [[T.StrictnessInfo]]
type S40Strictnesses31 =
    '(:) '[] S40Strictnesses32 :: [[T.StrictnessInfo]]
type S40Strictnesses30 =
    '(:) '[] S40Strictnesses31 :: [[T.StrictnessInfo]]
type S40Strictnesses29 =
    '(:) '[] S40Strictnesses30 :: [[T.StrictnessInfo]]
type S40Strictnesses28 =
    '(:) '[] S40Strictnesses29 :: [[T.StrictnessInfo]]
type S40Strictnesses27 =
    '(:) '[] S40Strictnesses28 :: [[T.StrictnessInfo]]
type S40Strictnesses26 =
    '(:) '[] S40Strictnesses27 :: [[T.StrictnessInfo]]
type S40Strictnesses25 =
    '(:) '[] S40Strictnesses26 :: [[T.StrictnessInfo]]
type S40Strictnesses24 =
    '(:) '[] S40Strictnesses25 :: [[T.StrictnessInfo]]
type S40Strictnesses23 =
    '(:) '[] S40Strictnesses24 :: [[T.StrictnessInfo]]
type S40Strictnesses22 =
    '(:) '[] S40Strictnesses23 :: [[T.StrictnessInfo]]
type S40Strictnesses21 =
    '(:) '[] S40Strictnesses22 :: [[T.StrictnessInfo]]
type S40Strictnesses20 =
    '(:) '[] S40Strictnesses21 :: [[T.StrictnessInfo]]
type S40Strictnesses19 =
    '(:) '[] S40Strictnesses20 :: [[T.StrictnessInfo]]
type S40Strictnesses18 =
    '(:) '[] S40Strictnesses19 :: [[T.StrictnessInfo]]
type S40Strictnesses17 =
    '(:) '[] S40Strictnesses18 :: [[T.StrictnessInfo]]
type S40Strictnesses16 =
    '(:) '[] S40Strictnesses17 :: [[T.StrictnessInfo]]
type S40Strictnesses15 =
    '(:) '[] S40Strictnesses16 :: [[T.StrictnessInfo]]
type S40Strictnesses14 =
    '(:) '[] S40Strictnesses15 :: [[T.StrictnessInfo]]
type S40Strictnesses13 =
    '(:) '[] S40Strictnesses14 :: [[T.StrictnessInfo]]
type S40Strictnesses12 =
    '(:) '[] S40Strictnesses13 :: [[T.StrictnessInfo]]
type S40Strictnesses11 =
    '(:) '[] S40Strictnesses12 :: [[T.StrictnessInfo]]
type S40Strictnesses10 =
    '(:) '[] S40Strictnesses11 :: [[T.StrictnessInfo]]
type S40Strictnesses9 =
    '(:) '[] S40Strictnesses10 :: [[T.StrictnessInfo]]
type S40Strictnesses8 =
    '(:) '[] S40Strictnesses9 :: [[T.StrictnessInfo]]
type S40Strictnesses7 =
    '(:) '[] S40Strictnesses8 :: [[T.StrictnessInfo]]
type S40Strictnesses6 =
    '(:) '[] S40Strictnesses7 :: [[T.StrictnessInfo]]
type S40Strictnesses5 =
    '(:) '[] S40Strictnesses6 :: [[T.StrictnessInfo]]
type S40Strictnesses4 =
    '(:) '[] S40Strictnesses5 :: [[T.StrictnessInfo]]
type S40Strictnesses3 =
    '(:) '[] S40Strictnesses4 :: [[T.StrictnessInfo]]
type S40Strictnesses2 =
    '(:) '[] S40Strictnesses3 :: [[T.StrictnessInfo]]
type S40Strictnesses1 =
    '(:) '[] S40Strictnesses2 :: [[T.StrictnessInfo]]
type S40Strictnesses =
    '(:) '[] S40Strictnesses1 :: [[T.StrictnessInfo]]
instance HasDatatypeInfo S40 where
  type DatatypeInfoOf S40 = 'T.ADT "CoreSize.S40.THNoSig" "S40" S40Constructors S40Strictnesses
  datatypeInfo _
    = T.demoteDatatypeInfo
        (Proxy ::
           Proxy (DatatypeInfoOf S40))
