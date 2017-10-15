{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}
-- | n-ary sums (and sums of products)
module Generics.SOP.NS
  ( -- * Datatypes
    NS(..)
  , SOP(..)
  , unSOP
    -- * Constructing sums
  , Injection
  , injections
  , shift
  , shiftInjection
  , apInjs_NP
  , apInjs'_NP
  , apInjs_POP
  , apInjs'_POP
    -- * Destructing sums
  , unZ
  , index_NS
  , index_SOP
    -- * Application
  , ap_NS
  , ap_SOP
    -- * Lifting / mapping
  , liftA_NS
  , liftA_SOP
  , liftA2_NS
  , liftA2_SOP
  , cliftA_NS
  , cliftA_SOP
  , cliftA2_NS
  , cliftA2_SOP
  , map_NS
  , map_SOP
  , cmap_NS
  , cmap_SOP
    -- * Dealing with @'All' c@
  , cliftA2'_NS
    -- * Collapsing
  , collapse_NS
  , collapse_SOP
    -- * Folding
  , ctraverse__NS
  , ctraverse__SOP
  , cfoldMap_NS
  , cfoldMap_SOP
    -- * Sequencing
  , sequence'_NS
  , sequence'_SOP
  , sequence_NS
  , sequence_SOP
  , ctraverse'_NS
  , ctraverse'_SOP
  , ctraverse_NS
  , ctraverse_SOP
    -- * Catamorphism and anamorphism
  , cata_NS
  , ccata_NS
  , ana_NS
  , cana_NS
    -- * Expanding sums to products
  , expand_NS
  , cexpand_NS
  , expand_SOP
  , cexpand_SOP
    -- * Transformation of index lists and coercions
  , trans_NS
  , trans_SOP
  , coerce_NS
  , coerce_SOP
  , fromI_NS
  , fromI_SOP
  , toI_NS
  , toI_SOP
  ) where

#if !(MIN_VERSION_base(4,8,0))
import Control.Applicative
import Data.Monoid (Monoid)
#endif
import Data.Coerce
import Data.Proxy
import Unsafe.Coerce

import Control.DeepSeq (NFData(..))

import Generics.SOP.BasicFunctors
import Generics.SOP.Classes
import Generics.SOP.Constraint
import Generics.SOP.NP
import Generics.SOP.Sing

-- * Datatypes

-- | An n-ary sum.
--
-- The sum is parameterized by a type constructor @f@ and
-- indexed by a type-level list @xs@. The length of the list
-- determines the number of choices in the sum and if the
-- @i@-th element of the list is of type @x@, then the @i@-th
-- choice of the sum is of type @f x@.
--
-- The constructor names are chosen to resemble Peano-style
-- natural numbers, i.e., 'Z' is for "zero", and 'S' is for
-- "successor". Chaining 'S' and 'Z' chooses the corresponding
-- component of the sum.
--
-- /Examples:/
--
-- > Z         :: f x -> NS f (x ': xs)
-- > S . Z     :: f y -> NS f (x ': y ': xs)
-- > S . S . Z :: f z -> NS f (x ': y ': z ': xs)
-- > ...
--
-- Note that empty sums (indexed by an empty list) have no
-- non-bottom elements.
--
-- Two common instantiations of @f@ are the identity functor 'I'
-- and the constant functor 'K'. For 'I', the sum becomes a
-- direct generalization of the 'Either' type to arbitrarily many
-- choices. For @'K' a@, the result is a homogeneous choice type,
-- where the contents of the type-level list are ignored, but its
-- length specifies the number of options.
--
-- In the context of the SOP approach to generic programming, an
-- n-ary sum describes the top-level structure of a datatype,
-- which is a choice between all of its constructors.
--
-- /Examples:/
--
-- > Z (I 'x')      :: NS I       '[ Char, Bool ]
-- > S (Z (I True)) :: NS I       '[ Char, Bool ]
-- > S (Z (K 1))    :: NS (K Int) '[ Char, Bool ]
--
data NS :: (k -> *) -> [k] -> * where
  Z :: f x -> NS f (x ': xs)
  S :: NS f xs -> NS f (x ': xs)

deriving instance All (Show `Compose` f) xs => Show (NS f xs)
deriving instance All (Eq   `Compose` f) xs => Eq   (NS f xs)
deriving instance (All (Eq `Compose` f) xs, All (Ord `Compose` f) xs) => Ord (NS f xs)

-- | @since 0.2.5.0
instance All (NFData `Compose` f) xs => NFData (NS f xs) where
    rnf (Z x)  = rnf x
    rnf (S xs) = rnf xs

-- | Extract the payload from a unary sum.
--
-- For larger sums, this function would be partial, so it is only
-- provided with a rather restrictive type.
--
-- /Example:/
--
-- >>> unZ (Z (I 'x'))
-- I 'x'
--
-- @since 0.2.2.0
--
unZ :: NS f '[x] -> f x
unZ (Z x) = x
unZ _     = error "inaccessible" -- needed even in GHC 8.0.1

-- | Obtain the index from an n-ary sum.
--
-- An n-nary sum represents a choice between n different options.
-- This function returns an integer between 0 and n - 1 indicating
-- the option chosen by the given value.
--
-- /Examples:/
--
-- >>> index_NS (S (S (Z (I False))))
-- 2
-- >>> index_NS (Z (K ()))
-- 0
--
-- @since 0.2.4.0
--
index_NS :: forall f xs . NS f xs -> Int
index_NS = go 0
  where
    go :: forall ys . Int -> NS f ys -> Int
    go !acc (Z _) = acc
    go !acc (S x) = go (acc + 1) x

instance HIndex NS where
  hindex = index_NS

-- | A sum of products.
--
-- This is a 'newtype' for an 'NS' of an 'NP'. The elements of the
-- (inner) products are applications of the parameter @f@. The type
-- 'SOP' is indexed by the list of lists that determines the sizes
-- of both the (outer) sum and all the (inner) products, as well as
-- the types of all the elements of the inner products.
--
-- An @'SOP' 'I'@ reflects the structure of a normal Haskell datatype.
-- The sum structure represents the choice between the different
-- constructors, the product structure represents the arguments of
-- each constructor.
--
newtype SOP (f :: (k -> *)) (xss :: [[k]]) = SOP (NS (NP f) xss)

deriving instance (Show (NS (NP f) xss)) => Show (SOP f xss)
deriving instance (Eq   (NS (NP f) xss)) => Eq   (SOP f xss)
deriving instance (Ord  (NS (NP f) xss)) => Ord  (SOP f xss)

-- | @since 0.2.5.0
instance (NFData (NS (NP f) xss)) => NFData (SOP f xss) where
    rnf (SOP xss) = rnf xss

-- | Unwrap a sum of products.
unSOP :: SOP f xss -> NS (NP f) xss
unSOP (SOP xss) = xss

type instance AllN NS  c = All  c
type instance AllN SOP c = All2 c

-- | Obtain the index from an n-ary sum of products.
--
-- An n-nary sum represents a choice between n different options.
-- This function returns an integer between 0 and n - 1 indicating
-- the option chosen by the given value.
--
-- /Specification:/
--
-- @
-- 'index_SOP' = 'index_NS' '.' 'unSOP'
-- @
--
-- /Example:/
--
-- >>> index_SOP (SOP (S (Z (I True :* I 'x' :* Nil))))
-- 1
--
-- @since 0.2.4.0
--
index_SOP :: SOP f xs -> Int
index_SOP = index_NS . unSOP

instance HIndex SOP where
  hindex = index_SOP

-- * Constructing sums

-- | The type of injections into an n-ary sum.
--
-- If you expand the type synonyms and newtypes involved, you get
--
-- > Injection f xs a = (f -.-> K (NS f xs)) a ~= f a -> K (NS f xs) a ~= f a -> NS f xs
--
-- If we pick @a@ to be an element of @xs@, this indeed corresponds to an
-- injection into the sum.
--
type Injection (f :: k -> *) (xs :: [k]) = f -.-> K (NS f xs)

-- | Compute all injections into an n-ary sum.
--
-- Each element of the resulting product contains one of the injections.
--
injections :: forall xs f. SListI xs => NP (Injection f xs) xs
injections = case sList :: SList xs of
  SNil   -> Nil
  SCons  -> fn (K . Z) :* liftA_NP shiftInjection injections

-- | Shift an injection.
--
-- Given an injection, return an injection into a sum that is one component larger.
--
shiftInjection :: Injection f xs a -> Injection f (x ': xs) a
shiftInjection (Fn f) = Fn $ K . S . unK . f

{-# DEPRECATED shift "Use 'shiftInjection' instead." #-}
-- | Shift an injection.
--
-- Given an injection, return an injection into a sum that is one component larger.
--
shift :: Injection f xs a -> Injection f (x ': xs) a
shift = shiftInjection

-- | Apply injections to a product.
--
-- Given a product containing all possible choices, produce a
-- list of sums by applying each injection to the appropriate
-- element.
--
-- /Example:/
--
-- >>> apInjs_NP (I 'x' :* I True :* I 2 :* Nil)
-- [Z (I 'x'),S (Z (I True)),S (S (Z (I 2)))]
--
apInjs_NP  :: SListI xs  => NP  f xs  -> [NS  f xs]
apInjs_NP  = hcollapse . apInjs'_NP

-- | `apInjs_NP` without `hcollapse`.
--
-- >>> apInjs'_NP (I 'x' :* I True :* I 2 :* Nil)
-- K (Z (I 'x')) :* K (S (Z (I True))) :* K (S (S (Z (I 2)))) :* Nil
--
-- @since 0.2.5.0
--
apInjs'_NP :: SListI xs => NP f xs -> NP (K (NS f xs)) xs
apInjs'_NP = hap injections

-- | Apply injections to a product of product.
--
-- This operates on the outer product only. Given a product
-- containing all possible choices (that are products),
-- produce a list of sums (of products) by applying each
-- injection to the appropriate element.
--
-- /Example:/
--
-- >>> apInjs_POP (POP ((I 'x' :* Nil) :* (I True :* I 2 :* Nil) :* Nil))
-- [SOP (Z (I 'x' :* Nil)),SOP (S (Z (I True :* I 2 :* Nil)))]
--
apInjs_POP :: SListI xss => POP f xss -> [SOP f xss]
apInjs_POP = map SOP . apInjs_NP . unPOP

-- | `apInjs_POP` without `hcollapse`.
--
-- /Example:/
--
-- >>> apInjs'_POP (POP ((I 'x' :* Nil) :* (I True :* I 2 :* Nil) :* Nil))
-- K (SOP (Z (I 'x' :* Nil))) :* K (SOP (S (Z (I True :* I 2 :* Nil)))) :* Nil
--
-- @since 0.2.5.0
--
apInjs'_POP :: SListI xss => POP f xss -> NP (K (SOP f xss)) xss
apInjs'_POP = hmap (K . SOP . unK) . hap injections . unPOP

type instance UnProd NP  = NS
type instance UnProd POP = SOP

instance HApInjs NS where
  hapInjs = apInjs_NP

instance HApInjs SOP where
  hapInjs = apInjs_POP

-- * Application

-- | Specialization of 'hap'.
ap_NS :: NP (f -.-> g) xs -> NS f xs -> NS g xs
ap_NS (Fn f  :* _)   (Z x)   = Z (f x)
ap_NS (_     :* fs)  (S xs)  = S (ap_NS fs xs)
ap_NS _ _ = error "inaccessible"

-- | Specialization of 'hap'.
ap_SOP  :: POP (f -.-> g) xss -> SOP f xss -> SOP g xss
ap_SOP (POP fss') (SOP xss') = SOP (go fss' xss')
  where
    go :: NP (NP (f -.-> g)) xss -> NS (NP f) xss -> NS (NP g) xss
    go (fs :* _  ) (Z xs ) = Z (ap_NP fs  xs )
    go (_  :* fss) (S xss) = S (go    fss xss)
    go _           _       = error "inaccessible"

-- The definition of 'ap_SOP' is a more direct variant of
-- '_ap_SOP_spec'. The direct definition has the advantage
-- that it avoids the 'SListI' constraint.
_ap_SOP_spec :: SListI xss => POP (t -.-> f) xss -> SOP t xss -> SOP f xss
_ap_SOP_spec (POP fs) (SOP xs) = SOP (liftA2_NS ap_NP fs xs)

type instance Same NS  = NS
type instance Same SOP = SOP

type instance Prod NS  = NP
type instance Prod SOP = POP

type instance SListIN NS  = SListI
type instance SListIN SOP = SListI2

instance HAp NS  where hap = ap_NS
instance HAp SOP where hap = ap_SOP

-- * Lifting / mapping

-- | Specialization of 'hliftA'.
liftA_NS  :: SListI     xs  => (forall a. f a -> g a) -> NS  f xs  -> NS  g xs
-- | Specialization of 'hliftA'.
liftA_SOP :: All SListI xss => (forall a. f a -> g a) -> SOP f xss -> SOP g xss

liftA_NS  = hliftA
liftA_SOP = hliftA

-- | Specialization of 'hliftA2'.
liftA2_NS  :: SListI     xs  => (forall a. f a -> g a -> h a) -> NP  f xs  -> NS  g xs  -> NS   h xs
-- | Specialization of 'hliftA2'.
liftA2_SOP :: All SListI xss => (forall a. f a -> g a -> h a) -> POP f xss -> SOP g xss -> SOP  h xss

liftA2_NS  = hliftA2
liftA2_SOP = hliftA2

-- | Specialization of 'hmap', which is equivalent to 'hliftA'.
map_NS  :: SListI     xs  => (forall a. f a -> g a) -> NS  f xs  -> NS  g xs
-- | Specialization of 'hmap', which is equivalent to 'hliftA'.
map_SOP :: All SListI xss => (forall a. f a -> g a) -> SOP f xss -> SOP g xss

map_NS  = hmap
map_SOP = hmap

-- | Specialization of 'hcliftA'.
cliftA_NS  :: All  c xs  => proxy c -> (forall a. c a => f a -> g a) -> NS   f xs  -> NS  g xs
-- | Specialization of 'hcliftA'.
cliftA_SOP :: All2 c xss => proxy c -> (forall a. c a => f a -> g a) -> SOP  f xss -> SOP g xss

cliftA_NS  = hcliftA
cliftA_SOP = hcliftA

-- | Specialization of 'hcliftA2'.
cliftA2_NS  :: All  c xs  => proxy c -> (forall a. c a => f a -> g a -> h a) -> NP  f xs  -> NS  g xs  -> NS  h xs
-- | Specialization of 'hcliftA2'.
cliftA2_SOP :: All2 c xss => proxy c -> (forall a. c a => f a -> g a -> h a) -> POP f xss -> SOP g xss -> SOP h xss

cliftA2_NS  = hcliftA2
cliftA2_SOP = hcliftA2

-- | Specialization of 'hcmap', which is equivalent to 'hcliftA'.
cmap_NS  :: All  c xs  => proxy c -> (forall a. c a => f a -> g a) -> NS   f xs  -> NS  g xs
-- | Specialization of 'hcmap', which is equivalent to 'hcliftA'.
cmap_SOP :: All2 c xss => proxy c -> (forall a. c a => f a -> g a) -> SOP  f xss -> SOP g xss

cmap_NS  = hcmap
cmap_SOP = hcmap

-- * Dealing with @'All' c@

-- | Specialization of 'hcliftA2''.
{-# DEPRECATED cliftA2'_NS "Use 'cliftA2_NS' instead." #-}
cliftA2'_NS :: All2 c xss => proxy c -> (forall xs. All c xs => f xs -> g xs -> h xs) -> NP f xss -> NS g xss -> NS h xss

cliftA2'_NS = hcliftA2'

-- * Collapsing

-- | Specialization of 'hcollapse'.
collapse_NS  ::               NS  (K a) xs  ->   a
-- | Specialization of 'hcollapse'.
collapse_SOP :: SListI xss => SOP (K a) xss ->  [a]

collapse_NS (Z (K x)) = x
collapse_NS (S xs)    = collapse_NS xs

collapse_SOP = collapse_NS . hliftA (K . collapse_NP) . unSOP

type instance CollapseTo NS  a =  a
type instance CollapseTo SOP a = [a]

instance HCollapse NS  where hcollapse = collapse_NS
instance HCollapse SOP where hcollapse = collapse_SOP

-- * Folding

-- | Specialization of 'hctraverse_'.
--
-- /Note:/ we don't need 'Applicative' constraint.
--
ctraverse__NS ::
     forall c proxy xs f g. (All c xs)
  => proxy c -> (forall a. c a => f a -> g ()) -> NS f xs -> g ()
ctraverse__NS _ f = go
  where
    go :: All c ys => NS f ys -> g ()
    go (Z x)  = f x
    go (S xs) = go xs

-- | Specialization of 'hcfoldMap'.
ctraverse__SOP ::
     forall c proxy xs f g. (All2 c xs, Applicative g)
  => proxy c -> (forall a. c a => f a -> g ()) -> SOP f xs -> g ()
ctraverse__SOP p f = ctraverse__NS (allP p) (ctraverse__NP p f) . unSOP

instance HFoldMap NS  where hctraverse_ = ctraverse__NS
instance HFoldMap SOP where hctraverse_ = ctraverse__SOP

-- | Specialization of 'hcfoldMap'.
--
-- /Note:/ We don't need 'Monoid' instance.
--
cfoldMap_NS ::
     forall c proxy f xs m. (All c xs)
  => proxy c -> (forall a. c a => f a -> m) -> NS f xs -> m
cfoldMap_NS _ f = go
  where
    go :: All c ys => NS f ys -> m
    go (Z x)  = f x
    go (S xs) = go xs

-- | Specialization of 'hcfoldMap'.
cfoldMap_SOP :: (All2 c xs, Monoid m) => proxy c -> (forall a. c a => f a -> m) -> SOP f xs -> m
cfoldMap_SOP = hcfoldMap

-- * Sequencing

-- | Specialization of 'hsequence''.
sequence'_NS  ::              Applicative f  => NS  (f :.: g) xs  -> f (NS  g xs)

-- | Specialization of 'hsequence''.
sequence'_SOP :: (SListI xss, Applicative f) => SOP (f :.: g) xss -> f (SOP g xss)

-- | Specialization of 'hctraverse''.
--
-- /Note:/ as 'NS' has exactly one element, the 'Functor' constraint is enough.
ctraverse'_NS  ::
     forall c proxy xs f f' g. (All c xs,  Functor g)
  => proxy c -> (forall a. c a => f a -> g (f' a)) -> NS f xs  -> g (NS f' xs)

-- | Specialization of 'hctraverse''.
ctraverse'_SOP :: (All2 c xs, Applicative g) => proxy c -> (forall a. c a => f a -> g (f' a)) -> SOP f xs -> g (SOP f' xs)

sequence'_NS (Z mx)  = Z <$> unComp mx
sequence'_NS (S mxs) = S <$> sequence'_NS mxs

sequence'_SOP = fmap SOP . sequence'_NS . hliftA (Comp . sequence'_NP) . unSOP

ctraverse'_NS _ f = go where
  go :: All c ys => NS f ys -> g (NS f' ys)
  go (Z x)  = Z <$> f x
  go (S xs) = S <$> go xs

ctraverse'_SOP p f = fmap SOP . ctraverse'_NS (allP p) (ctraverse'_NP p f) . unSOP

instance HSequence NS  where
  hsequence' = sequence'_NS
  hctraverse' = ctraverse'_NS
instance HSequence SOP where
  hsequence' = sequence'_SOP
  hctraverse' = ctraverse'_SOP

-- | Specialization of 'hsequence'.
sequence_NS  :: (SListI xs,  Applicative f) => NS  f xs  -> f (NS  I xs)

-- | Specialization of 'hsequence'.
sequence_SOP :: (All SListI xss, Applicative f) => SOP f xss -> f (SOP I xss)

sequence_NS   = hsequence
sequence_SOP  = hsequence

-- | Specialization of 'hctraverse'.
--
ctraverse_NS  :: (All  c xs, Applicative g) => proxy c -> (forall a. c a => f a -> g a) -> NP  f xs -> g (NP  I xs)

-- | Specialization of 'hctraverse'.
ctraverse_SOP :: (All2 c xs, Applicative g) => proxy c -> (forall a. c a => f a -> g a) -> POP f xs -> g (POP I xs)

ctraverse_NS = hctraverse
ctraverse_SOP = hctraverse

-- * Catamorphism and anamorphism

-- | Catamorphism for 'NS'.
--
-- Takes arguments determining what to do for 'Z'
-- and what to do for 'S'. The result type is still
-- indexed over the type-level lit.
--
-- @since 0.2.3.0
--
cata_NS ::
     forall r f xs .
     (forall y ys . f y -> r (y ': ys))
  -> (forall y ys . r ys -> r (y ': ys))
  -> NS f xs
  -> r xs
cata_NS z s = go
  where
    go :: forall ys . NS f ys -> r ys
    go (Z x) = z x
    go (S i) = s (go i)

-- | Constrained catamorphism for 'NS'.
--
-- @since 0.2.3.0
--
ccata_NS ::
     forall c proxy r f xs . (All c xs)
  => proxy c
  -> (forall y ys . c y => f y -> r (y ': ys))
  -> (forall y ys . c y => r ys -> r (y ': ys))
  -> NS f xs
  -> r xs
ccata_NS _ z s = go
  where
    go :: forall ys . (All c ys) => NS f ys -> r ys
    go (Z x) = z x
    go (S i) = s (go i)

-- | Anamorphism for 'NS'.
--
-- @since 0.2.3.0
--
ana_NS ::
     forall s f xs . (SListI xs)
  => (forall r . s '[] -> r)
  -> (forall y ys . s (y ': ys) -> Either (f y) (s ys))
  -> s xs
  -> NS f xs
ana_NS refute decide = go sList
  where
    go :: forall ys . SList ys -> s ys -> NS f ys
    go SNil  s = refute s
    go SCons s = case decide s of
      Left x   -> Z x
      Right s' -> S (go sList s')

-- | Constrained anamorphism for 'NS'.
--
-- @since 0.2.3.0
--
cana_NS :: forall c proxy s f xs .
     (All c xs)
  => proxy c
  -> (forall r . s '[] -> r)
  -> (forall y ys . c y => s (y ': ys) -> Either (f y) (s ys))
  -> s xs
  -> NS f xs
cana_NS _ refute decide = go sList
  where
    go :: forall ys . (All c ys) => SList ys -> s ys -> NS f ys
    go SNil  s = refute s
    go SCons s = case decide s of
      Left x   -> Z x
      Right s' -> S (go sList s')

-- * Expanding sums to products

-- | Specialization of 'hexpand'.
--
-- @since 0.2.5.0
--
expand_NS :: forall f xs .
     (SListI xs)
  => (forall x . f x)
  -> NS f xs -> NP f xs
expand_NS d = go sList
  where
    go :: forall ys . SList ys -> NS f ys -> NP f ys
    go SCons (Z x) = x :* hpure d
    go SCons (S i) = d :* go sList i
    go SNil  _     = error "inaccessible" -- still required in ghc-8.0.*

-- | Specialization of 'hcexpand'.
--
-- @since 0.2.5.0
--
cexpand_NS :: forall c proxy f xs .
     (All c xs)
  => proxy c -> (forall x . c x => f x)
  -> NS f xs -> NP f xs
cexpand_NS p d = go
  where
    go :: forall ys . All c ys => NS f ys -> NP f ys
    go (Z x) = x :* hcpure p d
    go (S i) = d :* go i

-- | Specialization of 'hexpand'.
--
-- @since 0.2.5.0
--
expand_SOP :: forall f xss .
     (All SListI xss)
  => (forall x . f x)
  -> SOP f xss -> POP f xss
expand_SOP d =
  POP . cexpand_NS (Proxy :: Proxy SListI) (hpure d) . unSOP

-- | Specialization of 'hcexpand'.
--
-- @since 0.2.5.0
--
cexpand_SOP :: forall c proxy f xss .
     (All2 c xss)
  => proxy c -> (forall x . c x => f x)
  -> SOP f xss -> POP f xss
cexpand_SOP p d =
  POP . cexpand_NS (allP p) (hcpure p d) . unSOP

allP :: proxy c -> Proxy (All c)
allP _ = Proxy

instance HExpand NS where
  hexpand  = expand_NS
  hcexpand = cexpand_NS

instance HExpand SOP where
  hexpand  = expand_SOP
  hcexpand = cexpand_SOP

-- | Specialization of 'htrans'.
--
-- @since 0.3.1.0
--
trans_NS ::
     AllZip c xs ys
  => proxy c
  -> (forall x y . c x y => f x -> g y)
  -> NS f xs -> NS g ys
trans_NS _ t (Z x)      = Z (t x)
trans_NS p t (S x)      = S (trans_NS p t x)

-- | Specialization of 'htrans'.
--
-- @since 0.3.1.0
--
trans_SOP ::
     AllZip2 c xss yss
  => proxy c
  -> (forall x y . c x y => f x -> g y)
  -> SOP f xss -> SOP g yss
trans_SOP p t =
  SOP . trans_NS (allZipP p) (trans_NP p t) . unSOP

allZipP :: proxy c -> Proxy (AllZip c)
allZipP _ = Proxy

-- | Specialization of 'hcoerce'.
--
-- @since 0.3.1.0
--
coerce_NS ::
     forall f g xs ys .
     AllZip (LiftedCoercible f g) xs ys
  => NS f xs -> NS g ys
coerce_NS =
  unsafeCoerce

-- There is a bug in the way coerce works for higher-kinded
-- type variables that seems to occur only in GHC 7.10.
--
-- Therefore, the safe versions of the coercion functions
-- are excluded below. This is harmless because they're only
-- present for documentation purposes and not exported.

#if __GLASGOW_HASKELL__ < 710 || __GLASGOW_HASKELL__ >= 800
_safe_coerce_NS ::
     forall f g xs ys .
     AllZip (LiftedCoercible f g) xs ys
  => NS f xs -> NS g ys
_safe_coerce_NS =
  trans_NS (Proxy :: Proxy (LiftedCoercible f g)) coerce
#endif

-- | Specialization of 'hcoerce'.
--
-- @since 0.3.1.0
--
coerce_SOP ::
     forall f g xss yss .
     AllZip2 (LiftedCoercible f g) xss yss
  => SOP f xss -> SOP g yss
coerce_SOP =
  unsafeCoerce

#if __GLASGOW_HASKELL__ < 710 || __GLASGOW_HASKELL__ >= 800
_safe_coerce_SOP ::
     forall f g xss yss .
     AllZip2 (LiftedCoercible f g) xss yss
  => SOP f xss -> SOP g yss
_safe_coerce_SOP =
  trans_SOP (Proxy :: Proxy (LiftedCoercible f g)) coerce
#endif

-- | Specialization of 'hfromI'.
--
-- @since 0.3.1.0
--
fromI_NS ::
     forall f xs ys .
     AllZip (LiftedCoercible I f) xs ys
  => NS I xs -> NS f ys
fromI_NS = hfromI

-- | Specialization of 'htoI'.
--
-- @since 0.3.1.0
--
toI_NS ::
     forall f xs ys .
     AllZip (LiftedCoercible f I) xs ys
  => NS f xs -> NS I ys
toI_NS = htoI

-- | Specialization of 'hfromI'.
--
-- @since 0.3.1.0
--
fromI_SOP ::
     forall f xss yss .
     AllZip2 (LiftedCoercible I f) xss yss
  => SOP I xss -> SOP f yss
fromI_SOP = hfromI

-- | Specialization of 'htoI'.
--
-- @since 0.3.1.0
--
toI_SOP ::
     forall f xss yss .
     AllZip2 (LiftedCoercible f I) xss yss
  => SOP f xss -> SOP I yss
toI_SOP = htoI

instance HTrans NS NS where
  htrans  = trans_NS
  hcoerce = coerce_NS

instance HTrans SOP SOP where
  htrans  = trans_SOP
  hcoerce = coerce_SOP
