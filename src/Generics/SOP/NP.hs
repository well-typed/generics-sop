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
    -- * Destructing products
  , hd
  , tl
  , Projection
  , projections
  , shiftProjection
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
    -- * Catamorphism and anamorphism
  , cata_NP
  , ccata_NP
  , ana_NP
  , cana_NP
    -- * Transformation of index lists and coercions
  , trans_NP
  , trans_POP
  , coerce_NP
  , coerce_POP
  , fromI_NP
  , fromI_POP
  , toI_NP
  , toI_POP
  ) where

#if !(MIN_VERSION_base(4,8,0))
import Control.Applicative
#endif
import Data.Coerce
import Data.Proxy (Proxy(..))
import GHC.Base
import Unsafe.Coerce

import Control.DeepSeq (NFData(..))

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
  (:*) :: f x -> !(NP f xs) -> NP f (x ': xs)

infixr 5 :*

-- This is written manually,
-- because built-in deriving doesn't use associativity information!
instance All (Show `Compose` f) xs => Show (NP f xs) where
  showsPrec _ Nil       = showString "Nil"
  showsPrec d (f :* fs) = showParen (d > 5)
    $ showsPrec (5 + 1) f
    . showString " :* "
    . showsPrec 5 fs

deriving instance All (Eq   `Compose` f) xs => Eq   (NP f xs)
deriving instance (All (Eq `Compose` f) xs, All (Ord `Compose` f) xs) => Ord (NP f xs)

-- | @since 0.2.5.0
instance All (NFData `Compose` f) xs => NFData (NP f xs) where
  rnf = unK . ccata_NP (Proxy :: Proxy (NFData `Compose` f)) (K ()) cons
    where
      cons x (K xs) = rnf x `seq` rnf xs `seq` K ()
      {-# INLINE cons #-}
  {-# INLINE rnf #-}

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

-- | @since 0.2.5.0
instance (NFData (NP (NP f) xss)) => NFData (POP f xss) where
    rnf (POP xss) = rnf xss

-- | Unwrap a product of products.
unPOP :: POP f xss -> NP (NP f) xss
unPOP (POP xss) = xss

type instance AllN NP  c = All  c
type instance AllN POP c = All2 c

type instance AllZipN NP  c = AllZip  c
type instance AllZipN POP c = AllZip2 c

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
pure_NP f =
  cataSList Nil (f :*)
{-# INLINE pure_NP #-}

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
cpure_NP p f =
  ccataSList p Nil (f :*)
{-# INLINE cpure_NP #-}

-- | Specialization of 'hcpure'.
--
-- The call @'cpure_NP' p x@ generates a product of products that contains 'x'
-- in every element position.
--
cpure_POP :: forall c xss proxy f. All2 c xss
          => proxy c -> (forall a. c a => f a) -> POP f xss
cpure_POP p f = POP (cpure_NP (allP p) (cpure_NP p f))
{-# INLINE cpure_POP #-}

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
fromList :: forall xs a . SListI xs => [a] -> Maybe (NP (K a) xs)
fromList =
  unComp . unComp $ cataSList
    (coerce nil)
    (coerce cons)
  where
    nil :: [a] -> Maybe (NP (K a) '[])
    nil [] = Just Nil
    nil _  = Nothing

    cons ::
         forall y ys .
         ([a] -> Maybe (NP (K a) ys))
      -> ([a] -> Maybe (NP (K a) (y ': ys)))
    cons _ []       = Nothing
    cons r (x : xs) = fmap (K x :*) (r xs)
{-# INLINE fromList #-}

-- * Application

-- | Specialization of 'hap'.
--
-- Applies a product of (lifted) functions pointwise to a product of
-- suitable arguments.
--
ap_NP :: SListI xs => NP (f -.-> g) xs -> NP f xs -> NP g xs
ap_NP =
  apFn_2 (cataSList (fn_2 nil) (fn_2 . cons . apFn_2))
  where
    nil :: NP (f -.-> g) '[] -> NP f '[] -> NP g '[]
    nil Nil Nil = Nil
    {-# INLINE nil #-}

    cons ::
         (NP (f -.-> g) ys -> NP f ys -> NP g ys)
      -> (NP (f -.-> g) (y ': ys) -> NP f (y ': ys) -> NP g (y ': ys))
    cons r (Fn f :* fs) (x :* xs) = f x :* r fs xs
    {-# INLINE cons #-}
{-# INLINE ap_NP #-}

-- | Specialization of 'hap'.
--
-- Applies a product of (lifted) functions pointwise to a product of
-- suitable arguments.
--
ap_POP :: All SListI xss => POP (f -.-> g) xss -> POP  f xss -> POP  g xss
ap_POP (POP fs) (POP xs) = POP (czipWith_NP (Proxy :: Proxy SListI) ap_NP fs xs)
{-# INLINE ap_POP #-}

-- The definition of 'ap_POP' is a more direct variant of
-- '_ap_POP_spec'. The direct definition has the advantage
-- that it avoids the 'SListI' constraint.

type instance Same NP  = NP
type instance Same POP = POP

type instance Prod NP  = NP
type instance Prod POP = POP

instance HAp NP  where hap = ap_NP
instance HAp POP where hap = ap_POP

-- * Destructing products

-- | Obtain the head of an n-ary product.
--
-- @since 0.2.1.0
--
hd :: NP f (x ': xs) -> f x
hd (x :* _xs) = x
{-# INLINE hd #-}

-- | Obtain the tail of an n-ary product.
--
-- @since 0.2.1.0
--
tl :: NP f (x ': xs) -> NP f xs
tl (_x :* xs) = xs
{-# INLINE tl #-}

-- | The type of projections from an n-ary product.
--
type Projection (f :: k -> *) (xs :: [k]) = K (NP f xs) -.-> f

-- | Compute all projections from an n-ary product.
--
-- Each element of the resulting product contains one of the projections.
--
projections :: forall xs f . SListI xs => NP (Projection f xs) xs
projections =
  unProjection_ (cataSList (Projection_ Nil) (Projection_ . cons . unProjection_))
  where
    cons r = fn (hd . unK) :* map_NP shiftProjection r
    {-# INLINE cons #-}
{-# INLINE projections #-}

newtype Projection_ f xs = Projection_ { unProjection_ :: NP (Projection f xs) xs }

shiftProjection :: Projection f xs a -> Projection f (x ': xs) a
shiftProjection (Fn f) = Fn $ f . K . tl . unK
{-# INLINE shiftProjection #-}

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
{-# DEPRECATED hcliftA' "Use 'hcliftA' or 'hcmap' instead." #-}
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
collapse_NP  :: SListI xs => NP  (K a) xs  ->  [a]

-- | Specialization of 'hcollapse'.
--
-- /Example:/
--
-- >>> collapse_POP (POP ((K 'a' :* Nil) :* (K 'b' :* K 'c' :* Nil) :* Nil) :: POP (K Char) '[ '[(a :: *)], '[b, c] ])
-- ["a","bc"]
--
-- (The type signature is only necessary in this case to fix the kind of the type variables.)
--
collapse_POP :: All SListI xss => POP (K a) xss -> [[a]]

collapse_NP np =
  build $ \ cons nil ->
  unK $ apFn
    (cataSList
      (fn (\ Nil -> K nil))
      (fn . (\ r (K x :* xs) -> K (x `cons` unK (r xs))) . apFn)
    )
  np
{-# INLINE collapse_NP #-}

collapse_POP = collapse_NP . hcmap (Proxy :: Proxy SListI) (K . collapse_NP) . unPOP

type instance CollapseTo NP  a = [a]
type instance CollapseTo POP a = [[a]]

instance HCollapse NP  where hcollapse = collapse_NP
instance HCollapse POP where hcollapse = collapse_POP

-- * Sequencing

-- | Specialization of 'hsequence''.
sequence'_NP  :: (SListI  xs , Applicative f) => NP  (f :.: g) xs  -> f (NP  g xs)

-- | Specialization of 'hsequence''.
sequence'_POP :: (SListI2 xss, Applicative f) => POP (f :.: g) xss -> f (POP g xss)

sequence'_NP = apFnM
  (cataSList
    (FnM (\ Nil -> pure Nil))
    (FnM . (\ r (mx :* mxs) -> (:*) <$> unComp mx <*> r mxs) . apFnM)
  )
{-# INLINE sequence'_NP #-}

sequence'_POP = fmap POP . sequence'_NP . hcmap (Proxy :: Proxy SListI) (Comp . sequence'_NP) . unPOP
{-# INLINE sequence'_POP #-}

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
-- Just (POP ((I 1 :* Nil) :* (I 2 :* I 3 :* Nil) :* Nil))
--
sequence_POP :: (All SListI xss, Applicative f) => POP f xss -> f (POP I xss)

sequence_NP   = hsequence
sequence_POP  = hsequence

-- * Catamorphism and anamorphism

-- | Catamorphism for 'NP'.
--
-- This is a suitable generalization of 'foldr'. It takes
-- parameters on what to do for 'Nil' and ':*'. Since the
-- input list is heterogeneous, the result is also indexed
-- by a type-level list.
--
-- @since 0.2.3.0
--
cata_NP ::
     forall r f xs . SListI xs
  => r '[]
  -> (forall y ys . f y -> r ys -> r (y ': ys))
  -> NP f xs
  -> r xs
cata_NP nil cons =
  apFn
    (cataSList
      (fn (\ Nil -> nil))
      (fn . (\ r (x :* xs) -> cons x (r xs)) . apFn)
    )
{-# INLINE cata_NP #-}

-- | Constrained catamorphism for 'NP'.
--
-- The difference compared to 'cata_NP' is that the function
-- for the cons-case can make use of the fact that the specified
-- constraint holds for all the types in the signature of the
-- product.
--
-- @since 0.2.3.0
--
ccata_NP ::
     forall c proxy r f xs . (All c xs)
  => proxy c
  -> r '[]
  -> (forall y ys . c y => f y -> r ys -> r (y ': ys))
  -> NP f xs
  -> r xs
ccata_NP p nil cons =
  apFn
    (ccataSList p
      (fn (\ Nil -> nil))
      (fn . (\ r (x :* xs) -> cons x (r xs)) . apFn)
    )
{-# INLINE ccata_NP #-}

-- | Anamorphism for 'NP'.
--
-- In contrast to the anamorphism for normal lists, the
-- generating function does not return an 'Either', but
-- simply an element and a new seed value.
--
-- This is because the decision on whether to generate a
-- 'Nil' or a ':*' is determined by the types.
--
-- @since 0.2.3.0
--
ana_NP ::
     forall s f xs .
     SListI xs
  => (forall y ys . s (y ': ys) -> (f y, s ys))
  -> s xs
  -> NP f xs
ana_NP uncons =
  apFn
    (cataSList
      (fn (\ _ -> Nil))
      (fn . (\ r s -> case uncons s of (x, s') -> x :* r s') . apFn)
    )
{-# INLINE ana_NP #-}

-- | Constrained anamorphism for 'NP'.
--
-- Compared to 'ana_NP', the generating function can
-- make use of the specified constraint here for the
-- elements that it generates.
--
-- @since 0.2.3.0
--
cana_NP ::
     forall c proxy s f xs . (All c xs)
  => proxy c
  -> (forall y ys . (c y, All c ys) => s (y ': ys) -> (f y, s ys))
  -> s xs
  -> NP f xs
cana_NP p uncons =
  apFn
    (ccataSList p
      (fn (const Nil))
      (fn . (\ r s -> case uncons s of (x, s') -> x :* r s') . apFn)
    )
{-# INLINE cana_NP #-}

-- | Specialization of 'htrans'.
--
-- @since 0.3.1.0
--
trans_NP ::
     AllZip c xs ys
  => proxy c
  -> (forall x y . c x y => f x -> g y)
  -> NP f xs -> NP g ys
trans_NP p t =
  apTrans (ccataSList2 p
    (Trans (\ Nil -> Nil))
    (Trans . (\ r (x :* xs) -> t x :* r xs) . apTrans)
  )
{-# INLINE trans_NP #-}

-- | Specialization of 'htrans'.
--
-- @since 0.3.1.0
--
trans_POP ::
     AllZip2 c xss yss
  => proxy c
  -> (forall x y . c x y => f x -> g y)
  -> POP f xss -> POP g yss
trans_POP p t =
  POP . trans_NP (allZipP p) (trans_NP p t) . unPOP
{-# INLINE trans_POP #-}

allZipP :: proxy c -> Proxy (AllZip c)
allZipP _ = Proxy

-- | Specialization of 'hcoerce'.
--
-- @since 0.3.1.0
--
coerce_NP ::
     forall f g xs ys .
     AllZip (LiftedCoercible f g) xs ys
  => NP f xs -> NP g ys
coerce_NP =
  unsafeCoerce
{-# INLINE coerce_NP #-}

-- There is a bug in the way coerce works for higher-kinded
-- type variables that seems to occur only in GHC 7.10.
--
-- Therefore, the safe versions of the coercion functions
-- are excluded below. This is harmless because they're only
-- present for documentation purposes and not exported.

#if __GLASGOW_HASKELL__ < 710 || __GLASGOW_HASKELL__ >= 800
_safe_coerce_NP ::
     forall f g xs ys .
     AllZip (LiftedCoercible f g) xs ys
  => NP f xs -> NP g ys
_safe_coerce_NP =
  trans_NP (Proxy :: Proxy (LiftedCoercible f g)) coerce
#endif

-- | Specialization of 'hcoerce'.
--
-- @since 0.3.1.0
--
coerce_POP ::
     forall f g xss yss .
     AllZip2 (LiftedCoercible f g) xss yss
  => POP f xss -> POP g yss
coerce_POP =
  unsafeCoerce
{-# INLINE coerce_POP #-}

#if __GLASGOW_HASKELL__ < 710 || __GLASGOW_HASKELL__ >= 800
_safe_coerce_POP ::
     forall f g xss yss .
     AllZip2 (LiftedCoercible f g) xss yss
  => POP f xss -> POP g yss
_safe_coerce_POP =
  trans_POP (Proxy :: Proxy (LiftedCoercible f g)) coerce
#endif

-- | Specialization of 'hfromI'.
--
-- @since 0.3.1.0
--
fromI_NP ::
     forall f xs ys .
     AllZip (LiftedCoercible I f) xs ys
  => NP I xs -> NP f ys
fromI_NP = hfromI
{-# INLINE fromI_NP #-}

-- | Specialization of 'htoI'.
--
-- @since 0.3.1.0
--
toI_NP ::
     forall f xs ys .
     AllZip (LiftedCoercible f I) xs ys
  => NP f xs -> NP I ys
toI_NP = htoI
{-# INLINE toI_NP #-}

-- | Specialization of 'hfromI'.
--
-- @since 0.3.1.0
--
fromI_POP ::
     forall f xss yss .
     AllZip2 (LiftedCoercible I f) xss yss
  => POP I xss -> POP f yss
fromI_POP = hfromI
{-# INLINE fromI_POP #-}

-- | Specialization of 'htoI'.
--
-- @since 0.3.1.0
--
toI_POP ::
     forall f xss yss .
     AllZip2 (LiftedCoercible f I) xss yss
  => POP f xss -> POP I yss
toI_POP = htoI
{-# INLINE toI_POP #-}

instance HTrans NP NP where
  htrans  = trans_NP
  hcoerce = coerce_NP
instance HTrans POP POP where
  htrans  = trans_POP
  hcoerce = coerce_POP
