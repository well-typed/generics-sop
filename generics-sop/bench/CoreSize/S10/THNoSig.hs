{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
-- | What @deriveGeneric@ splices into "CoreSize.S10.TH", pasted in with the
-- instance signatures of @from@ and @to@ dropped and the generated names
-- shortened. What this module costs over that one is what those signatures
-- buy: with them the methods are typed against the @S10Code@ synonym,
-- without them against @Code S10@.
module CoreSize.S10.THNoSig where

import Data.Kind (Type)
import Generics.SOP
import qualified Generics.SOP.Type.Metadata as T

data S10 =
    S10_0
  | S10_1
  | S10_2
  | S10_3
  | S10_4
  | S10_5
  | S10_6
  | S10_7
  | S10_8
  | S10_9

type S10Code9 = '[ '[]] :: [[Type]]
type S10Code8 =
    '(:) '[] S10Code9 :: [[Type]]
type S10Code7 =
    '(:) '[] S10Code8 :: [[Type]]
type S10Code6 =
    '(:) '[] S10Code7 :: [[Type]]
type S10Code5 =
    '(:) '[] S10Code6 :: [[Type]]
type S10Code4 =
    '(:) '[] S10Code5 :: [[Type]]
type S10Code3 =
    '(:) '[] S10Code4 :: [[Type]]
type S10Code2 =
    '(:) '[] S10Code3 :: [[Type]]
type S10Code1 =
    '(:) '[] S10Code2 :: [[Type]]
type S10Code =
    '(:) '[] S10Code1 :: [[Type]]
instance Generic S10 where
  type Code S10 = S10Code
  from S10_0
    = SOP (Z Nil)
  from S10_1
    = SOP (S (Z Nil))
  from S10_2
    = SOP
        (S (S (Z Nil)))
  from S10_3
    = SOP
        (S
           (S (S (Z Nil))))
  from S10_4
    = SOP
        (S
           (S
              (S (S (Z Nil)))))
  from S10_5
    = SOP
        (S
           (S
              (S
                 (S (S (Z Nil))))))
  from S10_6
    = SOP
        (S
           (S
              (S
                 (S
                    (S (S (Z Nil)))))))
  from S10_7
    = SOP
        (S
           (S
              (S
                 (S
                    (S
                       (S
                          (S (Z Nil))))))))
  from S10_8
    = SOP
        (S
           (S
              (S
                 (S
                    (S
                       (S
                          (S
                             (S (Z Nil)))))))))
  from S10_9
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
  to
    (SOP (Z Nil))
    = S10_0
  to
    (SOP (S (Z Nil)))
    = S10_1
  to
    (SOP (S (S (Z Nil))))
    = S10_2
  to
    (SOP (S (S (S (Z Nil)))))
    = S10_3
  to
    (SOP (S (S (S (S (Z Nil))))))
    = S10_4
  to
    (SOP (S (S (S (S (S (Z Nil)))))))
    = S10_5
  to
    (SOP (S (S (S (S (S (S (Z Nil))))))))
    = S10_6
  to
    (SOP (S (S (S (S (S (S (S (Z Nil)))))))))
    = S10_7
  to
    (SOP (S (S (S (S (S (S (S (S (Z Nil))))))))))
    = S10_8
  to
    (SOP (S (S (S (S (S (S (S (S (S (Z Nil)))))))))))
    = S10_9
  to
    (SOP (S (S (S (S (S (S (S (S (S (S x)))))))))))
    = (x `seq` error "inaccessible")
type S10ConstructorS10_0 =
    'T.Constructor "S10_0" :: T.ConstructorInfo
type S10ConstructorS10_1 =
    'T.Constructor "S10_1" :: T.ConstructorInfo
type S10ConstructorS10_2 =
    'T.Constructor "S10_2" :: T.ConstructorInfo
type S10ConstructorS10_3 =
    'T.Constructor "S10_3" :: T.ConstructorInfo
type S10ConstructorS10_4 =
    'T.Constructor "S10_4" :: T.ConstructorInfo
type S10ConstructorS10_5 =
    'T.Constructor "S10_5" :: T.ConstructorInfo
type S10ConstructorS10_6 =
    'T.Constructor "S10_6" :: T.ConstructorInfo
type S10ConstructorS10_7 =
    'T.Constructor "S10_7" :: T.ConstructorInfo
type S10ConstructorS10_8 =
    'T.Constructor "S10_8" :: T.ConstructorInfo
type S10ConstructorS10_9 =
    'T.Constructor "S10_9" :: T.ConstructorInfo
type S10Constructors9 =
    '[S10ConstructorS10_9] :: [T.ConstructorInfo]
type S10Constructors8 =
    '(:) S10ConstructorS10_8 S10Constructors9 :: [T.ConstructorInfo]
type S10Constructors7 =
    '(:) S10ConstructorS10_7 S10Constructors8 :: [T.ConstructorInfo]
type S10Constructors6 =
    '(:) S10ConstructorS10_6 S10Constructors7 :: [T.ConstructorInfo]
type S10Constructors5 =
    '(:) S10ConstructorS10_5 S10Constructors6 :: [T.ConstructorInfo]
type S10Constructors4 =
    '(:) S10ConstructorS10_4 S10Constructors5 :: [T.ConstructorInfo]
type S10Constructors3 =
    '(:) S10ConstructorS10_3 S10Constructors4 :: [T.ConstructorInfo]
type S10Constructors2 =
    '(:) S10ConstructorS10_2 S10Constructors3 :: [T.ConstructorInfo]
type S10Constructors1 =
    '(:) S10ConstructorS10_1 S10Constructors2 :: [T.ConstructorInfo]
type S10Constructors =
    '(:) S10ConstructorS10_0 S10Constructors1 :: [T.ConstructorInfo]
type S10Strictnesses9 =
    '[ '[]] :: [[T.StrictnessInfo]]
type S10Strictnesses8 =
    '(:) '[] S10Strictnesses9 :: [[T.StrictnessInfo]]
type S10Strictnesses7 =
    '(:) '[] S10Strictnesses8 :: [[T.StrictnessInfo]]
type S10Strictnesses6 =
    '(:) '[] S10Strictnesses7 :: [[T.StrictnessInfo]]
type S10Strictnesses5 =
    '(:) '[] S10Strictnesses6 :: [[T.StrictnessInfo]]
type S10Strictnesses4 =
    '(:) '[] S10Strictnesses5 :: [[T.StrictnessInfo]]
type S10Strictnesses3 =
    '(:) '[] S10Strictnesses4 :: [[T.StrictnessInfo]]
type S10Strictnesses2 =
    '(:) '[] S10Strictnesses3 :: [[T.StrictnessInfo]]
type S10Strictnesses1 =
    '(:) '[] S10Strictnesses2 :: [[T.StrictnessInfo]]
type S10Strictnesses =
    '(:) '[] S10Strictnesses1 :: [[T.StrictnessInfo]]
instance HasDatatypeInfo S10 where
  type DatatypeInfoOf S10 = 'T.ADT "CoreSize.S10.THNoSig" "S10" S10Constructors S10Strictnesses
  datatypeInfo _
    = T.demoteDatatypeInfo
        (Proxy ::
           Proxy (DatatypeInfoOf S10))
