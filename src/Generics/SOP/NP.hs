{-# LANGUAGE PolyKinds, StandaloneDeriving, UndecidableInstances #-}
-- | n-ary products (and products of products)
module Generics.SOP.NP
  ( -- * Datatypes
    NP(..)
  , POP(..)
  , unPOP
    -- * Constructing products
  , pure_NP
  , pure_POP
  , cpure_NP
  , cpure_POP
    -- ** Construction from a list
  , fromList
    -- * Application
  , ap_NP
  , ap_POP
    -- * Lifting / mapping
  , liftA_NP
  , liftA_POP
  , liftA2_NP
  , liftA2_POP
  , liftA3_NP
  , liftA3_POP
  , map_NP
  , map_POP
  , zipWith_NP
  , zipWith_POP
  , zipWith3_NP
  , zipWith3_POP
  , cliftA_NP
  , cliftA_POP
  , cliftA2_NP
  , cliftA2_POP
  , cliftA3_NP
  , cliftA3_POP
  , cmap_NP
  , cmap_POP
  , czipWith_NP
  , czipWith_POP
  , czipWith3_NP
  , czipWith3_POP
    -- * Dealing with @'All' c@
  , hcliftA'
  , hcliftA2'
  , hcliftA3'
  , cliftA2'_NP
    -- * Collapsing
  , collapse_NP
  , collapse_POP
    -- * Sequencing
  , sequence'_NP
  , sequence'_POP
  , sequence_NP
  , sequence_POP
  ) where

#if !(MIN_VERSION_base(4,8,0))
import Control.Applicative
#endif
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

deriving instance All (Show `Compose` f) xs => Show (NP f xs)
deriving instance All (Eq   `Compose` f) xs => Eq   (NP f xs)
deriving instance (All (Eq `Compose` f) xs, All (Ord `Compose` f) xs) => Ord (NP f xs)

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

deriving instance (Show (NP (NP f) xss)) => Show (POP f xss)
deriving instance (Eq   (NP (NP f) xss)) => Eq   (POP f xss)
deriving instance (Ord  (NP (NP f) xss)) => Ord  (POP f xss)

-- | Unwrap a product of products.
unPOP :: POP f xss -> NP (NP f) xss
unPOP (POP xss) = xss

type instance AllN NP  c = All  c
type instance AllN POP c = All2 c

type instance SListIN NP  = SListI
type instance SListIN POP = SListI2

-- * Constructing products

-- | Specialization of 'hpure'.
--
-- The call @'pure_NP' x@ generates a product that contains 'x' in every
-- element position.
--
-- /Example:/
--
-- >>> pure_NP [] :: NP [] '[Char, Bool]
-- "" :* [] :* Nil
-- >>> pure_NP (K 0) :: NP (K Int) '[Double, Int, String]
-- K 0 :* K 0 :* K 0 :* Nil
--
pure_NP :: forall f xs. SListI xs => (forall a. f a) -> NP f xs
pure_NP f = case sList :: SList xs of
  SNil   -> Nil
  SCons  -> f :* pure_NP f

-- | Specialization of 'hpure'.
--
-- The call @'pure_POP' x@ generates a product of products that contains 'x'
-- in every element position.
--
pure_POP :: All SListI xss => (forall a. f a) -> POP f xss
pure_POP f = POP (cpure_NP sListP (pure_NP f))

sListP :: Proxy SListI
sListP = Proxy

-- | Specialization of 'hcpure'.
--
-- The call @'cpure_NP' p x@ generates a product that contains 'x' in every
-- element position.
--
cpure_NP :: forall c xs proxy f. All c xs
         => proxy c -> (forall a. c a => f a) -> NP f xs
cpure_NP p f = case sList :: SList xs of
  SNil   -> Nil
  SCons  -> f :* cpure_NP p f

-- | Specialization of 'hcpure'.
--
-- The call @'cpure_NP' p x@ generates a product of products that contains 'x'
-- in every element position.
--
cpure_POP :: forall c xss proxy f. All2 c xss
          => proxy c -> (forall a. c a => f a) -> POP f xss
cpure_POP p f = POP (cpure_NP (allP p) (cpure_NP p f))

allP :: proxy c -> Proxy (All c)
allP _ = Proxy

instance HPure NP where
  hpure  = pure_NP
  hcpure = cpure_NP

instance HPure POP where
  hpure  = pure_POP
  hcpure = cpure_POP

-- ** Construction from a list

-- | Construct a homogeneous n-ary product from a normal Haskell list.
--
-- Returns 'Nothing' if the length of the list does not exactly match the
-- expected size of the product.
--
fromList :: SListI xs => [a] -> Maybe (NP (K a) xs)
fromList = go sList
  where
    go :: SList xs -> [a] -> Maybe (NP (K a) xs)
    go SNil  []     = return Nil
    go SCons (x:xs) = do ys <- go sList xs ; return (K x :* ys)
    go _     _      = Nothing

-- * Application

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
ap_POP :: POP (f -.-> g) xss -> POP f xss -> POP g xss
ap_POP (POP fss') (POP xss') = POP (go fss' xss')
  where
    go :: NP (NP (f -.-> g)) xss -> NP (NP f) xss -> NP (NP g) xss
    go Nil         Nil         = Nil
    go (fs :* fss) (xs :* xss) = ap_NP fs xs :* go fss xss
    go _           _           = error "inaccessible"

-- The definition of 'ap_POP' is a more direct variant of
-- '_ap_POP_spec'. The direct definition has the advantage
-- that it avoids the 'SListI' constraint.
_ap_POP_spec :: SListI xss => POP (f -.-> g) xss -> POP  f xss -> POP  g xss
_ap_POP_spec (POP fs) (POP xs) = POP (liftA2_NP ap_NP fs xs)

type instance Prod NP  = NP
type instance Prod POP = POP

instance HAp NP  where hap = ap_NP
instance HAp POP where hap = ap_POP

-- * Lifting / mapping

-- | Specialization of 'hliftA'.
liftA_NP  :: SListI     xs  => (forall a. f a -> g a) -> NP  f xs  -> NP  g xs
-- | Specialization of 'hliftA'.
liftA_POP :: All SListI xss => (forall a. f a -> g a) -> POP f xss -> POP g xss

liftA_NP  = hliftA
liftA_POP = hliftA

-- | Specialization of 'hliftA2'.
liftA2_NP  :: SListI     xs  => (forall a. f a -> g a -> h a) -> NP  f xs  -> NP  g xs  -> NP   h xs
-- | Specialization of 'hliftA2'.
liftA2_POP :: All SListI xss => (forall a. f a -> g a -> h a) -> POP f xss -> POP g xss -> POP  h xss

liftA2_NP  = hliftA2
liftA2_POP = hliftA2

-- | Specialization of 'hliftA3'.
liftA3_NP  :: SListI     xs  => (forall a. f a -> g a -> h a -> i a) -> NP  f xs  -> NP  g xs  -> NP  h xs  -> NP  i xs
-- | Specialization of 'hliftA3'.
liftA3_POP :: All SListI xss => (forall a. f a -> g a -> h a -> i a) -> POP f xss -> POP g xss -> POP h xss -> POP i xss

liftA3_NP  = hliftA3
liftA3_POP = hliftA3

-- | Specialization of 'hmap', which is equivalent to 'hliftA'.
map_NP  :: SListI     xs  => (forall a. f a -> g a) -> NP  f xs  -> NP  g xs
-- | Specialization of 'hmap', which is equivalent to 'hliftA'.
map_POP :: All SListI xss => (forall a. f a -> g a) -> POP f xss -> POP g xss

map_NP  = hmap
map_POP = hmap

-- | Specialization of 'hzipWith', which is equivalent to 'hliftA2'.
zipWith_NP  :: SListI     xs  => (forall a. f a -> g a -> h a) -> NP  f xs  -> NP  g xs  -> NP   h xs
-- | Specialization of 'hzipWith', which is equivalent to 'hliftA2'.
zipWith_POP :: All SListI xss => (forall a. f a -> g a -> h a) -> POP f xss -> POP g xss -> POP  h xss

zipWith_NP  = hzipWith
zipWith_POP = hzipWith

-- | Specialization of 'hzipWith3', which is equivalent to 'hliftA3'.
zipWith3_NP  :: SListI     xs  => (forall a. f a -> g a -> h a -> i a) -> NP  f xs  -> NP  g xs  -> NP  h xs  -> NP  i xs
-- | Specialization of 'hzipWith3', which is equivalent to 'hliftA3'.
zipWith3_POP :: All SListI xss => (forall a. f a -> g a -> h a -> i a) -> POP f xss -> POP g xss -> POP h xss -> POP i xss

zipWith3_NP  = hzipWith3
zipWith3_POP = hzipWith3

-- | Specialization of 'hcliftA'.
cliftA_NP  :: All  c xs  => proxy c -> (forall a. c a => f a -> g a) -> NP   f xs  -> NP  g xs
-- | Specialization of 'hcliftA'.
cliftA_POP :: All2 c xss => proxy c -> (forall a. c a => f a -> g a) -> POP  f xss -> POP g xss

cliftA_NP  = hcliftA
cliftA_POP = hcliftA

-- | Specialization of 'hcliftA2'.
cliftA2_NP  :: All  c xs  => proxy c -> (forall a. c a => f a -> g a -> h a) -> NP  f xs  -> NP  g xs  -> NP  h xs
-- | Specialization of 'hcliftA2'.
cliftA2_POP :: All2 c xss => proxy c -> (forall a. c a => f a -> g a -> h a) -> POP f xss -> POP g xss -> POP h xss

cliftA2_NP  = hcliftA2
cliftA2_POP = hcliftA2

-- | Specialization of 'hcliftA3'.
cliftA3_NP  :: All  c xs  => proxy c -> (forall a. c a => f a -> g a -> h a -> i a) -> NP  f xs  -> NP  g xs  -> NP  h xs  -> NP  i xs
-- | Specialization of 'hcliftA3'.
cliftA3_POP :: All2 c xss => proxy c -> (forall a. c a => f a -> g a -> h a -> i a) -> POP f xss -> POP g xss -> POP h xss -> POP i xss

cliftA3_NP  = hcliftA3
cliftA3_POP = hcliftA3

-- | Specialization of 'hcmap', which is equivalent to 'hcliftA'.
cmap_NP  :: All  c xs  => proxy c -> (forall a. c a => f a -> g a) -> NP   f xs  -> NP  g xs
-- | Specialization of 'hcmap', which is equivalent to 'hcliftA'.
cmap_POP :: All2 c xss => proxy c -> (forall a. c a => f a -> g a) -> POP  f xss -> POP g xss

cmap_NP  = hcmap
cmap_POP = hcmap

-- | Specialization of 'hczipWith', which is equivalent to 'hcliftA2'.
czipWith_NP  :: All  c xs  => proxy c -> (forall a. c a => f a -> g a -> h a) -> NP  f xs  -> NP  g xs  -> NP  h xs
-- | Specialization of 'hczipWith', which is equivalent to 'hcliftA2'.
czipWith_POP :: All2 c xss => proxy c -> (forall a. c a => f a -> g a -> h a) -> POP f xss -> POP g xss -> POP h xss

czipWith_NP  = hczipWith
czipWith_POP = hczipWith

-- | Specialization of 'hczipWith3', which is equivalent to 'hcliftA3'.
czipWith3_NP  :: All  c xs  => proxy c -> (forall a. c a => f a -> g a -> h a -> i a) -> NP  f xs  -> NP  g xs  -> NP  h xs  -> NP  i xs
-- | Specialization of 'hczipWith3', which is equivalent to 'hcliftA3'.
czipWith3_POP :: All2 c xss => proxy c -> (forall a. c a => f a -> g a -> h a -> i a) -> POP f xss -> POP g xss -> POP h xss -> POP i xss

czipWith3_NP  = hczipWith3
czipWith3_POP = hczipWith3

-- * Dealing with @'All' c@

-- | Lift a constrained function operating on a list-indexed structure
-- to a function on a list-of-list-indexed structure.
--
-- This is a variant of 'hcliftA'.
--
-- /Specification:/
--
-- @
-- 'hcliftA'' p f xs = 'hpure' ('fn_2' $ \\ 'AllDictC' -> f) \` 'hap' \` 'allDict_NP' p \` 'hap' \` xs
-- @
--
-- /Instances:/
--
-- @
-- 'hcliftA'' :: 'All2' c xss => proxy c -> (forall xs. 'All' c xs => f xs -> f' xs) -> 'NP' f xss -> 'NP' f' xss
-- 'hcliftA'' :: 'All2' c xss => proxy c -> (forall xs. 'All' c xs => f xs -> f' xs) -> 'Generics.SOP.NS.NS' f xss -> 'Generics.SOP.NS.NS' f' xss
-- @
--
{-# DEPRECATED hcliftA' "Use 'hclift' or 'hcmap' instead." #-}
hcliftA'  :: (All2 c xss, Prod h ~ NP, HAp h) => proxy c -> (forall xs. All c xs => f xs -> f' xs)                                                       -> h f   xss -> h f'   xss

-- | Like 'hcliftA'', but for binary functions.
{-# DEPRECATED hcliftA2' "Use 'hcliftA2' or 'hczipWith' instead." #-}
hcliftA2' :: (All2 c xss, Prod h ~ NP, HAp h) => proxy c -> (forall xs. All c xs => f xs -> f' xs -> f'' xs)            -> Prod h f xss                  -> h f'  xss -> h f''  xss

-- | Like 'hcliftA'', but for ternay functions.
{-# DEPRECATED hcliftA3' "Use 'hcliftA3' or 'hczipWith3' instead." #-}
hcliftA3' :: (All2 c xss, Prod h ~ NP, HAp h) => proxy c -> (forall xs. All c xs => f xs -> f' xs -> f'' xs -> f''' xs) -> Prod h f xss -> Prod h f' xss -> h f'' xss -> h f''' xss

hcliftA'  p = hcliftA  (allP p)
hcliftA2' p = hcliftA2 (allP p)
hcliftA3' p = hcliftA3 (allP p)

-- | Specialization of 'hcliftA2''.
{-# DEPRECATED cliftA2'_NP "Use 'cliftA2_NP'  instead." #-}
cliftA2'_NP :: All2 c xss => proxy c -> (forall xs. All c xs => f xs -> g xs -> h xs) -> NP f xss -> NP g xss -> NP h xss

cliftA2'_NP = hcliftA2'

-- * Collapsing

-- | Specialization of 'hcollapse'.
--
-- /Example:/
--
-- >>> collapse_NP (K 1 :* K 2 :* K 3 :* Nil)
-- [1,2,3]
--
collapse_NP  ::              NP  (K a) xs  ->  [a]

-- | Specialization of 'hcollapse'.
--
-- /Example:/
--
-- >>> collapse_POP (POP ((K 'a' :* Nil) :* (K 'b' :* K 'c' :* Nil) :* Nil) :: POP (K Char) '[ '[(a :: *)], '[b, c] ])
-- ["a", "bc"]
--
-- (The type signature is only necessary in this case to fix the kind of the type variables.)
--
collapse_POP :: SListI xss => POP (K a) xss -> [[a]]

collapse_NP Nil         = []
collapse_NP (K x :* xs) = x : collapse_NP xs

collapse_POP = collapse_NP . hliftA (K . collapse_NP) . unPOP

type instance CollapseTo NP  a = [a]
type instance CollapseTo POP a = [[a]]

instance HCollapse NP  where hcollapse = collapse_NP
instance HCollapse POP where hcollapse = collapse_POP

-- * Sequencing

-- | Specialization of 'hsequence''.
sequence'_NP  ::             Applicative f  => NP  (f :.: g) xs  -> f (NP  g xs)

-- | Specialization of 'hsequence''.
sequence'_POP :: (SListI xss, Applicative f) => POP (f :.: g) xss -> f (POP g xss)

sequence'_NP Nil         = pure Nil
sequence'_NP (mx :* mxs) = (:*) <$> unComp mx <*> sequence'_NP mxs

sequence'_POP = fmap POP . sequence'_NP . hliftA (Comp . sequence'_NP) . unPOP

instance HSequence NP  where hsequence' = sequence'_NP
instance HSequence POP where hsequence' = sequence'_POP

-- | Specialization of 'hsequence'.
--
-- /Example:/
--
-- >>> sequence_NP (Just 1 :* Just 2 :* Nil)
-- Just (I 1 :* I 2 :* Nil)
--
sequence_NP  :: (SListI xs,  Applicative f) => NP  f xs  -> f (NP  I xs)

-- | Specialization of 'hsequence'.
--
-- /Example:/
--
-- >>> sequence_POP (POP ((Just 1 :* Nil) :* (Just 2 :* Just 3 :* Nil) :* Nil))
-- Just (POP ((I 1 :* Nil) :* ((I 2 :* (I 3 :* Nil)) :* Nil)))
--
sequence_POP :: (All SListI xss, Applicative f) => POP f xss -> f (POP I xss)

sequence_NP   = hsequence
sequence_POP  = hsequence

