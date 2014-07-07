{-# LANGUAGE PolyKinds, StandaloneDeriving, UndecidableInstances #-}
-- | n-ary products (and products of products)
module Generics.SOP.NP where

import Control.Applicative
import Data.Proxy (Proxy(..))

import Generics.SOP.BasicFunctors
import Generics.SOP.Classes
import Generics.SOP.Constraint
import Generics.SOP.Sing

-- | An n-ary product.
--
-- The product is parameterized by a type constructor @f@ and
-- indexed by a type-level list @xs@. The length of the list
-- determines the number of elements in the product, and if the
-- @i@-th element of the list is of type @x@, then the @i@-th
-- element of the product is of type @f x@.
--
-- The constructor names are chosen to resemble the names of the
-- list constructors.
--
-- Two common instantiations of @f@ are the identity functor 'I'
-- and the constant functor 'K'. For 'I', the product becomes a
-- heterogeneous list, where the type-level list describes the
-- types of its components. For @'K' a@, the product becomes a
-- homogeneous list, where the contents of the type-level list are
-- ignored, but its length still specifies the number of elements.
--
-- In the context of the SOP approach to generic programming, an
-- n-ary product describes the structure of the arguments of a
-- single data constructor.
--
-- /Examples:/
--
-- > I 'x'    :* I True  :* Nil  ::  NP I       '[ Char, Bool ]
-- > K 0      :* K 1     :* Nil  ::  NP (K Int) '[ Char, Bool ]
-- > Just 'x' :* Nothing :* Nil  ::  NP Maybe   '[ Char, Bool ]
--
data NP :: (k -> *) -> [k] -> * where
  Nil  :: NP f '[]
  (:*) :: f x -> NP f xs -> NP f (x ': xs)

infixr 5 :*

deriving instance All Show (Map f xs) => Show (NP f xs)
deriving instance All Eq   (Map f xs) => Eq   (NP f xs)
deriving instance (All Eq (Map f xs), All Ord (Map f xs)) => Ord (NP f xs)

-- | A product of products.
--
-- This is a 'newtype' for an 'NP' of an 'NP'. The elements of the
-- inner products are applications of the parameter @f@. The type
-- 'POP' is indexed by the list of lists that determines the lengths
-- of both the outer and all the inner products, as well as the types
-- of all the elements of the inner products.
--
-- A 'POP' is reminiscent of a two-dimensional table (but the inner
-- lists can all be of different length). In the context of the SOP
-- approach to generic programming, a 'POP' is useful to represent
-- information that is available for all arguments of all constructors
-- of a datatype.
--
newtype POP (f :: (k -> *)) (xss :: [[k]]) = POP (NP (NP f) xss)
  deriving (Show, Eq, Ord)

-- | Unwrap a product of products.
unPOP :: POP f xss -> NP (NP f) xss
unPOP (POP xss) = xss

type instance AllMap NP  c xs = All  c xs
type instance AllMap POP c xs = All2 c xs

{-------------------------------------------------------------------------------
  Constructing products
-------------------------------------------------------------------------------}

-- | Specialization of 'hpure'.
--
-- The call @'pure_NP' x@ generates a product that contains 'x' in every
-- element position.
--
-- /Example:/
--
-- > >>> pure_NP [] :: NP [] '[Char, Bool]
-- > "" :* [] :* Nil
-- > >>> pure_NP (K 0) :: NP (K Int) '[Double, Int, String]
-- > K 0 :* K 0 :* K 0 :* Nil
--
pure_NP :: forall f xs. SingI xs => (forall a. f a) -> NP f xs
pure_NP f = case sing :: Sing xs of
  SNil   -> Nil
  SCons  -> f :* pure_NP f

-- | Specialization of 'hpure'.
--
-- The call @'pure_POP' x@ generates a product of products that contains 'x'
-- in every element position.
--
pure_POP :: forall f xss. SingI xss => (forall a. f a) -> POP f xss
pure_POP f = case sing :: Sing xss of
  SNil   -> POP Nil
  SCons  -> POP (pure_NP f :* unPOP (pure_POP f))

-- | Specialization of 'hcpure'.
--
-- The call @'cpure_NP' p x@ generates a product that contains 'x' in every
-- element position.
--
cpure_NP :: forall c xs f. (All c xs, SingI xs)
         => Proxy c -> (forall a. c a => f a) -> NP f xs
cpure_NP p f = case sing :: Sing xs of
  SNil   -> Nil
  SCons  -> f :* cpure_NP p f

-- | Specialization of 'hcpure'.
--
-- The call @'cpure_NP' p x@ generates a product of products that contains 'x'
-- in every element position.
--
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

-- | Specialization of 'hap'.
--
-- Applies a product of (lifted) functions pointwise to a product of
-- suitable arguments.
--
ap_NP :: NP (f -.-> g) xs -> NP f xs -> NP g xs
ap_NP Nil           Nil        = Nil
ap_NP (Fn f :* fs)  (x :* xs)  = f x :* ap_NP fs xs
ap_NP _ _ = error "inaccessible"

-- | Specialization of 'hap'.
--
-- Applies a product of (lifted) functions pointwise to a product of
-- suitable arguments.
--
ap_POP  :: POP (f -.-> g) xs -> POP  f xs -> POP  g xs
ap_POP (POP Nil        ) (POP Nil        ) = POP Nil
ap_POP (POP (fs :* fss)) (POP (xs :* xss)) = POP (ap_NP fs xs :* unPOP (ap_POP (POP fss) (POP xss)))
ap_POP _ _ = error "inaccessible"

type instance Prod NP  = NP
type instance Prod POP = POP

instance HAp NP  where hap = ap_NP
instance HAp POP where hap = ap_POP

-- Specialized functions, mostly to keep in line with the paper:

-- | Specialization of 'hliftA'.
liftA_NP  :: SingI xs  => (forall a. f a -> g a) -> NP  f xs  -> NP  g xs
-- | Specialization of 'hliftA'.
liftA_POP :: SingI xss => (forall a. f a -> g a) -> POP f xss -> POP g xss

liftA_NP  = hliftA
liftA_POP = hliftA

-- | Specialization of 'hliftA2'.
liftA2_NP  :: SingI xs  => (forall a. f a -> g a -> h a) -> NP  f xs  -> NP  g xs  -> NP   h xs
-- | Specialization of 'hliftA2'.
liftA2_POP :: SingI xss => (forall a. f a -> g a -> h a) -> POP f xss -> POP g xss -> POP  h xss

liftA2_NP  = hliftA2
liftA2_POP = hliftA2

-- | Specialization of 'hliftA3'.
liftA3_NP  :: SingI xs  => (forall a. f a -> g a -> h a -> i a) -> NP  f xs  -> NP  g xs  -> NP  h xs  -> NP  i xs
-- | Specialization of 'hliftA3'.
liftA3_POP :: SingI xss => (forall a. f a -> g a -> h a -> i a) -> POP f xss -> POP g xss -> POP h xss -> POP i xss

liftA3_NP  = hliftA3
liftA3_POP = hliftA3

-- | Specialization of 'hcliftA'.
cliftA_NP  :: (All  c xs,  SingI xs)  => Proxy c -> (forall a. c a => f a -> g a) -> NP   f xs  -> NP  g xs
-- | Specialization of 'hcliftA'.
cliftA_POP :: (All2 c xss, SingI xss) => Proxy c -> (forall a. c a => f a -> g a) -> POP  f xss -> POP g xss

cliftA_NP  = hcliftA
cliftA_POP = hcliftA

-- | Specialization of 'hcliftA2'.
cliftA2_NP  :: (All  c xs,  SingI xs)  => Proxy c -> (forall a. c a => f a -> g a -> h a) -> NP  f xs  -> NP  g xs  -> NP  h xs
-- | Specialization of 'hcliftA2'.
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

