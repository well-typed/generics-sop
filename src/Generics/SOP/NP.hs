{-# LANGUAGE PolyKinds, StandaloneDeriving, UndecidableInstances #-}
-- | n-ary products (and products of products)
module Generics.SOP.NP where

import Control.Applicative
import Data.Proxy (Proxy(..))

import Generics.SOP.BasicFunctors
import Generics.SOP.Classes
import Generics.SOP.Constraint
import Generics.SOP.Sing

-- | n-ary product
data NP :: (k -> *) -> [k] -> * where
  Nil  :: NP f '[]
  (:*) :: f x -> NP f xs -> NP f (x ': xs)

infixr 5 :*

deriving instance All Show (Map f xs) => Show (NP f xs)
deriving instance All Eq   (Map f xs) => Eq   (NP f xs)
deriving instance (All Eq (Map f xs), All Ord (Map f xs)) => Ord (NP f xs)

-- | n-ary product-of-products
newtype POP (f :: (k -> *)) (xss :: [[k]]) = POP { unPOP :: NP (NP f) xss }
  deriving (Show, Eq, Ord)

type instance AllMap NP  c xs = All  c xs
type instance AllMap POP c xs = All2 c xs

{-------------------------------------------------------------------------------
  Constructing products
-------------------------------------------------------------------------------}

pure_NP :: forall f xs. SingI xs => (forall a. f a) -> NP f xs
pure_NP f = case sing :: Sing xs of
  SNil   -> Nil
  SCons  -> f :* pure_NP f

pure_POP :: forall f xss. SingI xss => (forall a. f a) -> POP f xss
pure_POP f = case sing :: Sing xss of
  SNil   -> POP Nil
  SCons  -> POP (pure_NP f :* unPOP (pure_POP f))

cpure_NP :: forall c xs f. (All c xs, SingI xs)
         => Proxy c -> (forall a. c a => f a) -> NP f xs
cpure_NP p f = case sing :: Sing xs of
  SNil   -> Nil
  SCons  -> f :* cpure_NP p f

cpure_POP :: forall c f xss. (All2 c xss, SingI xss)
          => Proxy c -> (forall a. c a => f a) -> POP f xss
cpure_POP p f = case sing :: Sing xss of
  SNil   -> POP Nil
  SCons  -> POP (cpure_NP p f :* unPOP (cpure_POP p f))

-- Generalization

instance HPure NP where
  hpure  = pure_NP
  hcpure = cpure_NP

instance HPure POP where
  hpure  = pure_POP
  hcpure = cpure_POP

ap_NP :: NP (f -.-> g) xs -> NP f xs -> NP g xs
ap_NP Nil           Nil        = Nil
ap_NP (Fn f :* fs)  (x :* xs)  = f x :* ap_NP fs xs
ap_NP _ _ = error "inaccessible"

ap_POP  :: POP (f -.-> g) xs -> POP  f xs -> POP  g xs
ap_POP (POP Nil        ) (POP Nil        ) = POP Nil
ap_POP (POP (fs :* fss)) (POP (xs :* xss)) = POP (ap_NP fs xs :* unPOP (ap_POP (POP fss) (POP xss)))
ap_POP _ _ = error "inaccessible"

type instance Prod NP  = NP
type instance Prod POP = POP

instance HAp NP  where hap = ap_NP
instance HAp POP where hap = ap_POP

-- Specialized functions, mostly to keep in line with the paper:

liftA_NP  :: SingI xs  => (forall a. f a -> g a) -> NP  f xs  -> NP  g xs
liftA_POP :: SingI xss => (forall a. f a -> g a) -> POP f xss -> POP g xss

liftA_NP  = hliftA
liftA_POP = hliftA

liftA2_NP  :: SingI xs  => (forall a. f a -> g a -> h a) -> NP  f xs  -> NP  g xs  -> NP   h xs
liftA2_POP :: SingI xss => (forall a. f a -> g a -> h a) -> POP f xss -> POP g xss -> POP  h xss

liftA2_NP  = hliftA2
liftA2_POP = hliftA2

cliftA_NP  :: (All  c xs,  SingI xs)  => Proxy c -> (forall a. c a => f a -> g a) -> NP   f xs  -> NP  g xs
cliftA_POP :: (All2 c xss, SingI xss) => Proxy c -> (forall a. c a => f a -> g a) -> POP  f xss -> POP g xss

cliftA_NP  = hcliftA
cliftA_POP = hcliftA

cliftA2_NP  :: (All  c xs,  SingI xs)  => Proxy c -> (forall a. c a => f a -> g a -> h a) -> NP  f xs  -> NP  g xs  -> NP  h xs
cliftA2_POP :: (All2 c xss, SingI xss) => Proxy c -> (forall a. c a => f a -> g a -> h a) -> POP f xss -> POP g xss -> POP h xss

cliftA2_NP  = hcliftA2
cliftA2_POP = hcliftA2

{-------------------------------------------------------------------------------
  Dealing with 'All c'
-------------------------------------------------------------------------------}

allDict_NP :: forall (c :: k -> Constraint) (xss :: [[k]]). (All2 c xss, SingI xss)
           => Proxy c -> NP (AllDict c) xss
allDict_NP p = case sing :: Sing xss of
  SNil  -> Nil
  SCons -> AllDictC :* allDict_NP p

hcliftA'  :: (All2 c xss, SingI xss, Prod h ~ NP, HAp h) => Proxy c -> (forall xs. (SingI xs, All c xs) => f xs -> f' xs)                                                       -> h f   xss -> h f'   xss
hcliftA2' :: (All2 c xss, SingI xss, Prod h ~ NP, HAp h) => Proxy c -> (forall xs. (SingI xs, All c xs) => f xs -> f' xs -> f'' xs)            -> Prod h f xss                  -> h f'  xss -> h f''  xss
hcliftA3' :: (All2 c xss, SingI xss, Prod h ~ NP, HAp h) => Proxy c -> (forall xs. (SingI xs, All c xs) => f xs -> f' xs -> f'' xs -> f''' xs) -> Prod h f xss -> Prod h f' xss -> h f'' xss -> h f''' xss

hcliftA'  p f xs       = hpure (fn_2 $ \AllDictC -> f) `hap` allDict_NP p `hap` xs
hcliftA2' p f xs ys    = hpure (fn_3 $ \AllDictC -> f) `hap` allDict_NP p `hap` xs `hap` ys
hcliftA3' p f xs ys zs = hpure (fn_4 $ \AllDictC -> f) `hap` allDict_NP p `hap` xs `hap` ys `hap` zs

-- Specializations, to keep in line with the paper

cliftA2'_NP :: (All2 c xss, SingI xss) => Proxy c -> (forall xs. (SingI xs, All c xs) => f xs -> g xs -> h xs) -> NP f xss -> NP g xss -> NP h xss

cliftA2'_NP = hcliftA2'

{-------------------------------------------------------------------------------
  Collapsing
-------------------------------------------------------------------------------}

collapse_NP  ::              NP  (K a) xs  ->  [a]
collapse_POP :: SingI xss => POP (K a) xss -> [[a]]

collapse_NP Nil         = []
collapse_NP (K x :* xs) = x : collapse_NP xs

collapse_POP = collapse_NP . hliftA (K . collapse_NP) . unPOP

-- Generalization

type instance CollapseTo NP  = []
type instance CollapseTo POP = ([] :.: [])

instance HCollapse NP  where hcollapse = collapse_NP
instance HCollapse POP where hcollapse = Comp . collapse_POP

{-------------------------------------------------------------------------------
  Sequencing
-------------------------------------------------------------------------------}

sequence'_NP  ::             Applicative f  => NP  (f :.: g) xs  -> f (NP  g xs)
sequence'_POP :: (SingI xss, Applicative f) => POP (f :.: g) xss -> f (POP g xss)

sequence'_NP Nil         = pure Nil
sequence'_NP (mx :* mxs) = (:*) <$> unComp mx <*> sequence'_NP mxs

sequence'_POP = fmap POP . sequence'_NP . hliftA (Comp . sequence'_NP) . unPOP

-- Generalization

instance HSequence NP  where hsequence' = sequence'_NP
instance HSequence POP where hsequence' = sequence'_POP

-- Specializations, to keep in line with the paper

sequence_NP  :: (SingI xs,  Applicative f) => NP  f xs  -> f (NP  I xs)
sequence_POP :: (SingI xss, Applicative f) => POP f xss -> f (POP I xss)

sequence_NP   = hsequence
sequence_POP  = hsequence

{-------------------------------------------------------------------------------
  Partial operations
-------------------------------------------------------------------------------}

fromList :: (SingI xs) => [a] -> Maybe (NP (K a) xs)
fromList = go sing
  where
    go :: Sing xs -> [a] -> Maybe (NP (K a) xs)
    go SNil  []     = return Nil
    go SCons (x:xs) = do ys <- go sing xs ; return (K x :* ys)
    go _     _      = Nothing

