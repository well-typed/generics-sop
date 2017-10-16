{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fshow-hole-constraints #-}
module Main where

import Data.Monoid
import Generics.SOP
import GHC.Proof
import qualified GHC.Generics as GHC

data MyPair a b = MyPair a b
  deriving (GHC.Generic, Generic, Show)

data MyTriple a b c = MyTriple a b c
  deriving (GHC.Generic, Generic, Show)

-- The above two types have GGP-derived Generic instances,
-- whereas the built-in triples have TH-derived Generic instances.

productFrom :: IsProductType a xs => a -> NP I xs
productFrom = unZ . unSOP . from

productTo :: IsProductType a xs => NP I xs -> a
productTo = to . SOP . Z

cpureTuple :: Proof
cpureTuple =
  productTo (hcpure (Proxy :: Proxy Monoid) (I mempty))
  ===
  ((Sum 0, Product 1, []) :: (Sum Int, Product Int, [Bool]))

cpureMyTriple :: Proof
cpureMyTriple =
  productTo (hcpure (Proxy :: Proxy Monoid) (I mempty))
  ===
  (MyTriple (Sum 0) (Product 1) [] :: MyTriple (Sum Int) (Product Int) [Bool])

cmapTuple :: Proof
cmapTuple =
  (concat . hcollapse . hcmap (Proxy :: Proxy Show) (mapIK show) . from)
  ===
  ((\ (x, y, z) -> concat [show x, show y, show z]) :: (Int, Bool, Char) -> String)

cmapMyTriple :: Proof
cmapMyTriple =
  (concat . hcollapse . hcmap (Proxy :: Proxy Show) (mapIK show) . from)
  ===
  ((\ (MyTriple x y z) -> concat [show x, show y, show z]) :: MyTriple Int Bool Char -> String)

czipWithTuple :: Proof
czipWithTuple =
  (\ x y -> productTo (hczipWith (Proxy :: Proxy Monoid)
    (mapIII mappend) (productFrom x) (productFrom y)))
  ===
  (\ (Sum x1, Product x2, x3) (Sum y1, Product y2, y3) ->
    (Sum (x1 + y1 :: Int), Product (x2 * y2 :: Int), x3 ++ y3))

czipWithMyTriple :: Proof
czipWithMyTriple =
  (\ x y -> productTo (hczipWith (Proxy :: Proxy Monoid)
    (mapIII mappend) (productFrom x) (productFrom y)))
  ===
  (\ (MyTriple (Sum x1) (Product x2) x3) (MyTriple (Sum y1) (Product y2) y3) ->
    MyTriple (Sum (x1 + y1 :: Int)) (Product (x2 * y2 :: Int)) (x3 ++ y3))

type Lens' s a =
  forall f . Functor f => (a -> f a) -> (s -> f s)

newtype WrappedLens s a =
  Wrap { unWrap :: Lens' s a }

newtype WrappedGetter s a =
  WG { unWG :: s -> a }

sop_ :: Lens' (SOP f xss) (NS (NP f) xss)
sop_ = \ wrap (SOP ns) -> SOP <$> wrap ns

z_ :: Lens' (NS (NP f) '[ xs ]) (NP f xs)
z_ = \ wrap (Z np) -> Z <$> wrap np

hd_ :: Lens' (NP f (x ': xs)) (f x)
hd_ = \ wrap (x :* xs) -> (:* xs) <$> wrap x

tl_ :: Lens' (NP f (x ': xs)) (NP f xs)
tl_ = \ wrap (x :* xs) -> (x :*) <$> wrap xs

i_ :: Lens' (I x) x
i_ = \ wrap (I x) -> I <$> wrap x

id_ :: Lens' x x
id_ = \ wrap x -> (\ y -> y) <$> wrap x

(%) :: WrappedLens s a -> WrappedLens a b -> WrappedLens s b
(%) = \ (Wrap l1) (Wrap l2) -> Wrap (l1 . l2)
{-# INLINE (%) #-}

infixr 9 %

(%%) :: WrappedGetter s a -> WrappedGetter a b -> WrappedGetter s b
(%%) = \ (WG l1) (WG l2) -> WG (l2 . l1)
{-# INLINE (%%) #-}

infixr 9 %%


rep_ :: Generic a => Lens' a (Rep a)
rep_ = \ wrap a -> to <$> wrap (from a)

productRep_ :: IsProductType a xs => Lens' a (NP I xs)
productRep_ = \ wrap a -> (\ x -> productTo x) <$> wrap (productFrom a)

newtype LensHelper xs = LensHelper { unLensHelper :: NP (WrappedLens (NP I xs)) xs }

lenses :: IsProductType a xs => NP (WrappedLens a) xs
lenses = hmap (Wrap productRep_ %) $ unLensHelper $ cataSList (LensHelper Nil) $
  \ (LensHelper rec) -> LensHelper $ (Wrap hd_ % Wrap i_) :* hmap (Wrap tl_ %) rec
{-# INLINE lenses #-}

newtype OpticHelper f xs = OpticHelper { unOpticHelper :: NP (f (NP I xs)) xs }

getters :: IsProductType a xs => NP (WrappedGetter a) xs
getters = hmap (WG productFrom %%) $ unOpticHelper $ cataSList (OpticHelper Nil) $
  \ (OpticHelper rec) -> OpticHelper $ WG (unI . hd) :* hmap (WG tl %%) rec
{-# INLINE getters #-}

{-
-- Note: This already fails! So we have a problem with lens
-- composition in general.

lensId :: Proof
lensId =
  Wrap sop_ % Wrap id_ === Wrap sop_
-}

{-
lensesMyPair :: Proof
lensesMyPair =
  lenses
  ===
  (Wrap (\ wrap (MyPair x y) -> (\ z -> MyPair z y) <$> wrap x)
  :* Wrap (\ wrap (MyPair x y) -> (\ z -> MyPair x z) <$> wrap y)
  :* Nil :: NP (WrappedLens (MyPair Int Char)) '[Int, Char])
-}

{-
lensesPair :: Proof
lensesPair =
  lenses
  ===
  (Wrap (\ wrap (x, y) -> (,y) <$> wrap x)
  :* Wrap (\ wrap (x, y) -> (x,) <$> wrap y)
  :* Nil :: NP (WrappedLens (Int, Char)) '[Int, Char])
-}

gettersTriple :: Proof
gettersTriple =
  getters
  ===
  (WG (\ (x, _, _) -> x)
  :* WG (\ (_, y, _) -> y)
  :* WG (\ (_, _, z) -> z)
  :* Nil :: NP (WrappedGetter (Int, Char, Bool)) '[Int, Char, Bool])

gettersMyPair :: Proof
gettersMyPair =
  getters
  ===
  (WG (\ (MyTriple x _ _) -> x)
  :* WG (\ (MyTriple _ y _) -> y)
  :* WG (\ (MyTriple _ _ z) -> z)
  :* Nil :: NP (WrappedGetter (MyTriple Int Char Bool)) '[Int, Char, Bool])

main :: IO ()
main = return ()
