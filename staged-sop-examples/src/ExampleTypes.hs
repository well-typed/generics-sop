{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module ExampleTypes where

import Control.DeepSeq
import Data.Text
import Data.Time
import qualified Generics.SOP.TH as SOP
import qualified Generics.SOP as SOP
import Generics.SOP.Staged
import qualified Generics.SOP.Staged.TH as SSOP
import qualified GHC.Generics as GHC

data Foo = Foo [Int] Ordering Text
  deriving GHC.Generic

SOP.deriveGeneric ''Foo
SSOP.deriveGeneric ''Foo

{-
instance SGeneric Foo where
  type SDescription Foo = SOP.Code Foo
  type Constraints c Foo = (c [Int], c Ordering, c Text)
  data ConstraintsD c Foo = CFoo !(Dict c [Int]) !(Dict c Ordering) !(Dict c Text)

  sfrom c k =
    [|| case $$c of
          Foo is o txt -> $$(k (SOP (Z (C [|| is ||] :* C [|| o ||] :* C [|| txt ||] :* Nil))))
    ||]

  sto (SOP (Z (C is :* C o :* C txt :* Nil))) = [|| Foo $$is $$o $$txt ||]

  constraints =
    CFoo Dict Dict Dict
  allC c =
    POP $
      (  Comp (C [|| case $$c of CFoo d _ _ -> d ||])
      :* Comp (C [|| case $$c of CFoo _ d _ -> d ||])
      :* Comp (C [|| case $$c of CFoo _ _ d -> d ||]) :* Nil) :* Nil
-}

instance NFData Foo where
  rnf (Foo is o txt) = rnf is `seq` rnf o `seq` rnf txt

foo :: Foo
foo = Foo [1,2] LT "foo"

data Tag = GenericsSOP | StagedSOP | StockDeriving | GHCGenerics | Manual

data Tree (tag :: Tag) a = Leaf a | Node (Tree tag a) (Tree tag a)
  deriving GHC.Generic

SOP.deriveGeneric ''Tree
SSOP.deriveGeneric ''Tree

{-
instance SGeneric (Tree tag a) where
  type SDescription (Tree tag a) = SOP.Code (Tree tag a)
  type Constraints c (Tree tag a) = (c a, c (Tree tag a))
  data ConstraintsD c (Tree tag a) = CTree !(Dict c a) !(Dict c (Tree tag a))

  sfrom c k =
    [|| case $$c of
          Leaf a   -> $$(k (SOP (Z (C [|| a ||] :* Nil))))
          Node l r -> $$(k (SOP (S (Z (C [|| l ||] :* C [|| r ||] :* Nil)))))
    ||]

  sto (SOP (Z (C a :* Nil)))            = [|| Leaf $$a ||]
  sto (SOP (S (Z (C l :* C r :* Nil)))) = [|| Node $$l $$r ||]

  constraints =
    CTree Dict Dict
  allC c =
    let
      da = Comp (C [|| case $$c of CTree d _ -> d ||])
      dt = Comp (C [|| case $$c of CTree _ d -> d ||])
    in
      POP $
           (da :* Nil)
        :* (dt :* dt :* Nil)
        :* Nil
-}

instance NFData a => NFData (Tree tag a) where
  rnf (Leaf a) = rnf a
  rnf (Node l r) = rnf l `seq` rnf r

data Pair a b = Pair a b
  deriving GHC.Generic

SOP.deriveGeneric ''Pair
SSOP.deriveGeneric ''Pair

{-
instance SGeneric (Pair a b) where
  type SDescription (Pair a b) = '[ '[ a, b ] ]
  type Constraints c (Pair a b) = (c a, c b)
  data ConstraintsD c (Pair a b) = CPair !(Dict c a) !(Dict c b)

  sfrom c k =
    [|| case $$c of
          Pair a b -> $$(k (SOP (Z (C [|| a ||] :* C [|| b ||] :* Nil))))
    ||]

  sto (SOP (Z (C a :* C b :* Nil))) = [|| Pair $$a $$b ||]

  constraints =
    CPair Dict Dict
  allC c =
    let
      da = Comp (C [|| case $$c of CPair d _ -> d ||])
      db = Comp (C [|| case $$c of CPair _ d -> d ||])
    in
      POP $
           (da :* db :* Nil)
        :* Nil
-}

instance (NFData a, NFData b) => NFData (Pair a b) where
  rnf (Pair a b) = rnf a `seq` rnf b

SSOP.deriveGeneric ''Ordering
{-
instance SGeneric Ordering where
  type SDescription Ordering = '[ '[], '[], '[] ]
  type Constraints c Ordering = ()
  data ConstraintsD c Ordering = COrdering

  sfrom c k =
    [|| case $$c of
          LT -> $$(k (SOP (Z Nil)))
          EQ -> $$(k (SOP (S (Z Nil))))
          GT -> $$(k (SOP (S (S (Z Nil)))))
    ||]

  sto (SOP (Z Nil))         = [|| LT ||]
  sto (SOP (S (Z Nil)))     = [|| EQ ||]
  sto (SOP (S (S (Z Nil)))) = [|| GT ||]

  constraints =
    COrdering
  allC _ =
    POP $ Nil :* Nil :* Nil :* Nil
-}

data Person = Person { personId :: Int, name :: String, date :: Day }
  deriving GHC.Generic

SOP.deriveGeneric ''Person
SSOP.deriveGeneric ''Person

{-
instance SGeneric Person where
  type SDescription Person = '[ '[ Int, String, Day ] ]
  type Constraints c Person = (c Int, c String, c Day)
  data ConstraintsD c Person = CPerson !(Dict c Int) !(Dict c String) !(Dict c Day)

  sfrom c k =
    [|| case $$c of
          Person i n d -> $$(k (SOP (Z (C [|| i ||] :* C [|| n ||] :* C [|| d ||] :* Nil))))
    ||]

  sto (SOP (Z (C i :* C n :* C d :* Nil))) = [|| Person $$i $$n $$d ||]

  constraints =
    CPerson Dict Dict Dict
  allC c =
    let
      di = Comp (C [|| case $$c of CPerson d _ _ -> d ||])
      ds = Comp (C [|| case $$c of CPerson _ d _ -> d ||])
      dd = Comp (C [|| case $$c of CPerson _ _ d -> d ||])
    in
      POP $
           (di :* ds :* dd :* Nil)
        :* Nil
-}

instance NFData Person where
  rnf (Person i n d) = rnf i `seq` rnf n `seq` rnf d

data Prop (tag :: Tag) =
    Var String
  | T
  | F
  | Not (Prop tag)
  | And (Prop tag) (Prop tag)
  | Or  (Prop tag) (Prop tag)
  deriving GHC.Generic

SOP.deriveGeneric ''Prop
SSOP.deriveGeneric ''Prop

{-
instance SGeneric (Prop tag) where
  type SDescription (Prop tag) = SOP.Code (Prop tag)
  type Constraints c (Prop tag) = (c String, c (Prop tag))
  data ConstraintsD c (Prop tag) = CProp !(Dict c String) !(Dict c (Prop tag))

  sfrom c k =
    [|| case $$c of
          Var s     -> $$(k (SOP (Z (C [|| s ||] :* Nil))))
          T         -> $$(k (SOP (S (Z Nil))))
          F         -> $$(k (SOP (S (S (Z Nil)))))
          Not p     -> $$(k (SOP (S (S (S (Z (C [|| p ||] :* Nil)))))))
          And p1 p2 -> $$(k (SOP (S (S (S (S (Z (C [|| p1 ||] :* C [|| p2 ||] :* Nil))))))))
          Or p1 p2  -> $$(k (SOP (S (S (S (S (S (Z (C [|| p1 ||] :* C [|| p2 ||] :* Nil)))))))))
    ||]

  sto (SOP (Z (C s :* Nil)))                              = [|| Var $$s ||]
  sto (SOP (S (Z Nil)))                                   = [|| T ||]
  sto (SOP (S (S (Z Nil))))                               = [|| F ||]
  sto (SOP (S (S (S (Z (C p :* Nil))))))                  = [|| Not $$p ||]
  sto (SOP (S (S (S (S (Z (C p1 :* C p2 :* Nil)))))))     = [|| And $$p1 $$p2 ||]
  sto (SOP (S (S (S (S (S (Z (C p1 :* C p2 :* Nil)))))))) = [|| Or $$p1 $$p2 ||]

  constraints =
    CProp Dict Dict
  allC c =
    let
      ds = Comp (C [|| case $$c of CProp d _ -> d ||])
      dp = Comp (C [|| case $$c of CProp _ d -> d ||])
    in
      POP $
           (ds :* Nil)
        :* Nil
        :* Nil
        :* (dp :* Nil)
        :* (dp :* dp :* Nil)
        :* (dp :* dp :* Nil)
        :* Nil
-}

instance NFData (Prop tag) where
  rnf (Var s) = rnf s
  rnf T = ()
  rnf F = ()
  rnf (Not p) = rnf p
  rnf (And p1 p2) = rnf p1 `seq` rnf p2
  rnf (Or p1 p2) =rnf p1 `seq` rnf p2

simple_prop :: Prop tag
simple_prop =
  Or (And (Var "x") (And (Not T) F)) (And (And (Not F) T) (Var "y"))

small_prop :: Prop tag
small_prop =
  And simple_prop simple_prop

medium_prop :: Prop tag
medium_prop =
  Or small_prop small_prop

large_prop :: Prop tag
large_prop =
  And medium_prop medium_prop

huge_prop :: Prop tag
huge_prop =
  Or large_prop large_prop

data S15 =
    S15_00
  | S15_01
  | S15_02
  | S15_03
  | S15_04
  | S15_05
  | S15_06
  | S15_07
  | S15_08
  | S15_09
  | S15_10
  | S15_11
  | S15_12
  | S15_13
  | S15_14
  deriving GHC.Generic

SOP.deriveGeneric ''S15
SSOP.deriveGeneric ''S15

{-
instance SGeneric S15 where
  type SDescription S15 = SOP.Code S15
  type Constraints c S15 = ()
  data ConstraintsD c S15 = CS15

  sfrom c k =
    [|| case $$c of
          S15_00 -> $$(k (SOP (Z Nil)))
          S15_01 -> $$(k (SOP (S (Z Nil))))
          S15_02 -> $$(k (SOP (S (S (Z Nil)))))
          S15_03 -> $$(k (SOP (S (S (S (Z Nil))))))
          S15_04 -> $$(k (SOP (S (S (S (S (Z Nil)))))))
          S15_05 -> $$(k (SOP (S (S (S (S (S (Z Nil))))))))
          S15_06 -> $$(k (SOP (S (S (S (S (S (S (Z Nil)))))))))
          S15_07 -> $$(k (SOP (S (S (S (S (S (S (S (Z Nil))))))))))
          S15_08 -> $$(k (SOP (S (S (S (S (S (S (S (S (Z Nil)))))))))))
          S15_09 -> $$(k (SOP (S (S (S (S (S (S (S (S (S (Z Nil))))))))))))
          S15_10 -> $$(k (SOP (S (S (S (S (S (S (S (S (S (S (Z Nil)))))))))))))
          S15_11 -> $$(k (SOP (S (S (S (S (S (S (S (S (S (S (S (Z Nil))))))))))))))
          S15_12 -> $$(k (SOP (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil)))))))))))))))
          S15_13 -> $$(k (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil))))))))))))))))
          S15_14 -> $$(k (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil)))))))))))))))))
    ||]

  sto (SOP (Z Nil))                                                         = [|| S15_00 ||]
  sto (SOP (S (Z Nil)))                                                     = [|| S15_01 ||]
  sto (SOP (S (S (Z Nil))))                                                 = [|| S15_02 ||]
  sto (SOP (S (S (S (Z Nil)))))                                             = [|| S15_03 ||]
  sto (SOP (S (S (S (S (Z Nil))))))                                         = [|| S15_04 ||]
  sto (SOP (S (S (S (S (S (Z Nil)))))))                                     = [|| S15_05 ||]
  sto (SOP (S (S (S (S (S (S (Z Nil))))))))                                 = [|| S15_06 ||]
  sto (SOP (S (S (S (S (S (S (S (Z Nil)))))))))                             = [|| S15_07 ||]
  sto (SOP (S (S (S (S (S (S (S (S (Z Nil))))))))))                         = [|| S15_08 ||]
  sto (SOP (S (S (S (S (S (S (S (S (S (Z Nil)))))))))))                     = [|| S15_09 ||]
  sto (SOP (S (S (S (S (S (S (S (S (S (S (Z Nil))))))))))))                 = [|| S15_10 ||]
  sto (SOP (S (S (S (S (S (S (S (S (S (S (S (Z Nil)))))))))))))             = [|| S15_11 ||]
  sto (SOP (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil))))))))))))))         = [|| S15_12 ||]
  sto (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil)))))))))))))))     = [|| S15_13 ||]
  sto (SOP (S (S (S (S (S (S (S (S (S (S (S (S (S (S (Z Nil)))))))))))))))) = [|| S15_14 ||]

  constraints =
    CS15
  allC _ =
    POP $
       Nil
    :* Nil
    :* Nil
    :* Nil
    :* Nil
    :* Nil
    :* Nil
    :* Nil
    :* Nil
    :* Nil
    :* Nil
    :* Nil
    :* Nil
    :* Nil
    :* Nil
    :* Nil
-}

tree :: Tree tag Int
tree = Node (Node (Leaf 1) (Leaf 2)) (Node (Leaf 3) (Leaf 4))

tree_medium :: Tree tag Int
tree_medium =
  Node (Node tree (Node tree tree)) (Node (Node tree tree) tree)

tree_large :: Tree tag Int
tree_large =
  Node
    (Node tree_medium (Node tree_medium tree_medium))
    (Node (Node tree_medium tree_medium) tree_medium)

