{-# LANGUAGE PatternSynonyms, PolyKinds, StandaloneDeriving, UndecidableInstances, ViewPatterns #-}
-- | n-ary products (and products of products)
module Generics.SOP.NP
  ( -- * Datatypes
    NP(.., Nil, (:*))
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
  ) where

#if !(MIN_VERSION_base(4,8,0))
import Control.Applicative
#endif
import Data.Proxy (Proxy(..))
import qualified Data.Vector as V
import GHC.Exts (Any)
import Unsafe.Coerce

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
newtype NP (f :: k -> *) (xs :: [k]) = NP (V.Vector (f Any))

from_NP_List :: NP_List f xs -> NP f xs
from_NP_List (NP_List xs) = NP (V.fromList xs)
{-# INLINE from_NP_List #-}

data IsNP (f :: k -> *) (xs :: [k]) where
  IsNil  :: IsNP f '[]
  IsCons :: f x -> NP f xs -> IsNP f (x ': xs)

isNP :: NP f xs -> IsNP f xs
isNP (NP xs) =
  if V.null xs
    then unsafeCoerce IsNil
    else unsafeCoerce (IsCons (V.unsafeHead xs) (NP (V.unsafeTail xs)))

pattern Nil :: () => (xs ~ '[]) => NP f xs
pattern Nil <- (isNP -> IsNil)
  where
    Nil = NP V.empty

pattern (:*) :: () => (xs' ~ (x ': xs)) => f x -> NP f xs -> NP f xs'
pattern x :* xs <- (isNP -> IsCons x xs)
  where
    x :* NP xs = NP (V.cons (unsafeCoerce x) xs)
infixr 5 :*

instance All (Show `Compose` f) xs => Show (NP f xs) where
  show xs =
    show (hcollapse (hcmap (Proxy :: Proxy (Show `Compose` f)) (K . Showable . show) xs))

newtype Showable = Showable String

instance Show Showable where
  show (Showable x) = x

instance All (Eq `Compose` f) xs => Eq (NP f xs) where
  xs == ys =
    and (hcollapse (hczipWith (Proxy :: Proxy (Eq `Compose` f)) (\ x y -> K (x == y)) xs ys))

instance (All (Eq `Compose` f) xs, All (Ord `Compose` f) xs) => Ord (NP f xs) where
  compare xs ys =
    mconcat (hcollapse (hczipWith (Proxy :: Proxy (Ord `Compose` f)) (\ x y -> K (compare x y)) xs ys))

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
pure_NP f = NP (V.replicate (lengthSList (Proxy :: Proxy xs)) (unsafeCoerce f))
{-
pure_NP f = case sList :: SList xs of
  SNil   -> Nil
  SCons  -> f :* pure_NP f
-}

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
cpure_NP _ x = from_NP_List (cpure_NP_List (Proxy :: Proxy c) x)

-- | Specialization of 'hcpure'.
--
-- The call @'cpure_NP' p x@ generates a product of products that contains 'x'
-- in every element position.
--
cpure_POP :: forall c xss proxy f. (All2 c xss)
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
fromList :: forall xs a . SListI xs => [a] -> Maybe (NP (K a) xs)
fromList xs =
  if length xs == lengthSList (Proxy :: Proxy xs)
    then Just (NP (V.fromList (unsafeCoerce xs)))
    else Nothing
    
{-
fromList = go sList
  where
    go :: SList xs -> [a] -> Maybe (NP (K a) xs)
    go SNil  []     = return Nil
    go SCons (x:xs) = do ys <- go sList xs ; return (K x :* ys)
    go _     _      = Nothing
-}

-- * Application

-- | Specialization of 'hap'.
--
-- Applies a product of (lifted) functions pointwise to a product of
-- suitable arguments.
--
ap_NP :: NP (f -.-> g) xs -> NP f xs -> NP g xs
ap_NP (NP fs) (NP xs) = NP (V.zipWith unsafeApply fs xs) 
  where
    unsafeApply f x = unsafeCoerce f x

-- | Specialization of 'hap'.
--
-- Applies a product of (lifted) functions pointwise to a product of
-- suitable arguments.
--
ap_POP :: POP (f -.-> g) xss -> POP f xss -> POP g xss
ap_POP (POP (NP fss)) (POP (NP xss)) =
  POP (NP (V.zipWith ap_NP fss xss))

-- The definition of 'ap_POP' is a more direct variant of
-- '_ap_POP_spec'. The direct definition has the advantage
-- that it avoids the 'SListI' constraint.
_ap_POP_spec :: SListI xss => POP (f -.-> g) xss -> POP  f xss -> POP  g xss
_ap_POP_spec (POP fs) (POP xs) = POP (liftA2_NP ap_NP fs xs)

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
hd (NP xs) = unsafeCoerce (V.head xs)

-- | Obtain the tail of an n-ary product.
--
-- @since 0.2.1.0
--
tl :: NP f (x ': xs) -> NP f xs
tl (NP xs) = unsafeCoerce (V.tail xs)

-- | The type of projections from an n-ary product.
--
type Projection (f :: k -> *) (xs :: [k]) = K (NP f xs) -.-> f

-- | Compute all projections from an n-ary product.
--
-- Each element of the resulting product contains one of the projections.
--
projections :: forall xs f . SListI xs => NP (Projection f xs) xs
projections = ana_NP (\ (K i) -> (Fn (\ (K (NP xs)) -> unsafeCoerce (xs V.! i)), K (i + 1))) (K 0)

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

collapse_NP (NP xs) = unsafeCoerce (V.toList xs)

collapse_POP = collapse_NP . hliftA (K . collapse_NP) . unPOP

type instance CollapseTo NP  a = [a]
type instance CollapseTo POP a = [[a]]

instance HCollapse NP  where hcollapse = collapse_NP
instance HCollapse POP where hcollapse = collapse_POP

-- * Sequencing

-- | Specialization of 'hsequence''.
sequence'_NP  :: forall f g xs . Applicative f  => NP (f :.: g) xs  -> f (NP  g xs)

-- | Specialization of 'hsequence''.
sequence'_POP :: (SListI xss, Applicative f) => POP (f :.: g) xss -> f (POP g xss)

sequence'_NP (NP mxs) =
  NP <$> sequenceA (unsafeCoerce mxs :: V.Vector (f (g Any)))
  -- the outer NP mapping could probably be an unsafeCoerce, too, with sufficient role constraints

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
     forall r f xs .
     r '[]
  -> (forall y ys . f y -> r ys -> r (y ': ys))
  -> NP f xs
  -> r xs
cata_NP nil cons (NP xs) =
  unsafeCoerce (V.foldr (unsafeCoerce cons) (unsafeCoerce nil) xs)

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
ccata_NP _p _nil _cons = error "TODO"
{-
ccata_NP _ nil cons = go
  where
    go :: forall ys . (All c ys) => NP f ys -> r ys
    go Nil       = nil
    go (x :* xs) = cons x (go xs)
-}

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
ana_NP uncons s =
  NP (V.fromList (ana_List (Proxy :: Proxy xs) uncons s))
  -- We could define this as a special case of cana_NP except we need allTop,
  -- and we cannot depend on the Dict module here without circular dependencies.

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
  -> (forall y ys . c y => s (y ': ys) -> (f y, s ys))
  -> s xs
  -> NP f xs
cana_NP _ uncons s = from_NP_List (cana_NP_List (Proxy :: Proxy c) uncons s)
