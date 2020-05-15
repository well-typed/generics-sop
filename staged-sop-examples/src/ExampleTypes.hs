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
import qualified GHC.Generics as GHC

data Foo = Foo [Int] Ordering Text
  deriving GHC.Generic

SOP.deriveGeneric ''Foo

instance SGeneric Foo where
  type SDescription Foo = SOP.Code Foo

  sfrom c k =
    [|| case $$c of
          Foo is o txt -> $$(k (SOP (Z (C [|| is ||] :* C [|| o ||] :* C [|| txt ||] :* Nil))))
    ||]

  sto (SOP (Z (C is :* C o :* C txt :* Nil))) = [|| Foo $$is $$o $$txt ||]

instance NFData Foo where
  rnf (Foo is o txt) = rnf is `seq` rnf o `seq` rnf txt

foo :: Foo
foo = Foo [1,2] LT "foo"

data Tag = GenericsSOP | StagedSOP | StockDeriving | GHCGenerics | Manual

data Tree (tag :: Tag) a = Leaf a | Node (Tree tag a) (Tree tag a)
  deriving GHC.Generic

SOP.deriveGeneric ''Tree

instance (LiftT a, LiftT tag) => SGeneric (Tree tag a) where
  type SDescription (Tree tag a) = SOP.Code (Tree tag a)

  sfrom c k =
    [|| case $$c of
          Leaf a   -> $$(k (SOP (Z (C [|| a ||] :* Nil))))
          Node l r -> $$(k (SOP (S (Z (C [|| l ||] :* C [|| r ||] :* Nil)))))
    ||]

  sto (SOP (Z (C a :* Nil)))            = [|| Leaf $$a ||]
  sto (SOP (S (Z (C l :* C r :* Nil)))) = [|| Node $$l $$r ||]

instance NFData a => NFData (Tree tag a) where
  rnf (Leaf a) = rnf a
  rnf (Node l r) = rnf l `seq` rnf r

data Pair a b = Pair a b
  deriving GHC.Generic

SOP.deriveGeneric ''Pair

instance (LiftT a, LiftT b) => SGeneric (Pair a b) where
  type SDescription (Pair a b) = '[ '[ a, b ] ]

  sfrom c k =
    [|| case $$c of
          Pair a b -> $$(k (SOP (Z (C [|| a ||] :* C [|| b ||] :* Nil))))
    ||]

  sto (SOP (Z (C a :* C b :* Nil))) = [|| Pair $$a $$b ||]

instance (NFData a, NFData b) => NFData (Pair a b) where
  rnf (Pair a b) = rnf a `seq` rnf b

instance SGeneric Ordering where
  type SDescription Ordering = '[ '[], '[], '[] ]

  sfrom c k =
    [|| case $$c of
          LT -> $$(k (SOP (Z Nil)))
          EQ -> $$(k (SOP (S (Z Nil))))
          GT -> $$(k (SOP (S (S (Z Nil)))))
    ||]

  sto (SOP (Z Nil))         = [|| LT ||]
  sto (SOP (S (Z Nil)))     = [|| EQ ||]
  sto (SOP (S (S (Z Nil)))) = [|| GT ||]

data Person = Person { personId :: Int, name :: String, date :: Day }
  deriving GHC.Generic

SOP.deriveGeneric ''Person

instance SGeneric Person where
  type SDescription Person = '[ '[ Int, String, Day ] ]

  sfrom c k =
    [|| case $$c of
          Person i n d -> $$(k (SOP (Z (C [|| i ||] :* C [|| n ||] :* C [|| d ||] :* Nil))))
    ||]

  sto (SOP (Z (C i :* C n :* C d :* Nil))) = [|| Person $$i $$n $$d ||]

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

instance LiftT tag => SGeneric (Prop tag) where
  type SDescription (Prop tag) = SOP.Code (Prop tag)

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

instance SGeneric S15 where
  type SDescription S15 = SOP.Code S15

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

