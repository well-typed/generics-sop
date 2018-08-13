{-# LANGUAGE PolyKinds #-}
-- | Classes for generalized combinators on SOP types.
--
-- In the SOP approach to generic programming, we're predominantly
-- concerned with four structured datatypes:
--
-- @
--   'Generics.SOP.NP.NP'  :: (k -> *) -> ( [k]  -> *)   -- n-ary product
--   'Generics.SOP.NS.NS'  :: (k -> *) -> ( [k]  -> *)   -- n-ary sum
--   'Generics.SOP.NP.POP' :: (k -> *) -> ([[k]] -> *)   -- product of products
--   'Generics.SOP.NS.SOP' :: (k -> *) -> ([[k]] -> *)   -- sum of products
-- @
--
-- All of these have a kind that fits the following pattern:
--
-- @
--   (k -> *) -> (l -> *)
-- @
--
-- These four types support similar interfaces. In order to allow
-- reusing the same combinator names for all of these types, we define
-- various classes in this module that allow the necessary
-- generalization.
--
-- The classes typically lift concepts that exist for kinds @*@ or
-- @* -> *@ to datatypes of kind @(k -> *) -> (l -> *)@. This module
-- also derives a number of derived combinators.
--
-- The actual instances are defined in "Generics.SOP.NP" and
-- "Generics.SOP.NS".
--
module Generics.SOP.Classes
  ( -- * Generalized applicative functor structure
    -- ** Generalized 'Control.Applicative.pure'
    HPure(..)
    -- ** Generalized 'Control.Applicative.<*>'
  , type (-.->)(..)
  , type (=.=>)(..)
  , type Trans(..)
  , fn
  , fn_2
  , fn_3
  , fn_4
  , apFn_2
  , Same
  , Prod
  , HAp(..)
    -- ** Derived functions
  , hliftA
  , hliftA2
  , hliftA3
  , hmap
  , hzipWith
  , hzipWith3
  , hcliftA
  , hcliftA2
  , hcliftA3
  , hcmap
  , hczipWith
  , hczipWith3
    -- * Collapsing homogeneous structures
  , CollapseTo
  , HCollapse(..)
    -- * Folding and sequencing
  , HTraverse_(..)
  , HSequence(..)
    -- ** Derived functions
  , hcfoldMap
  , hcfor_
  , hsequence
  , hsequenceK
  , hctraverse
  , hcfor
    -- * Indexing into sums
  , HIndex(..)
    -- * Applying all injections
  , UnProd
  , HApInjs(..)
    -- * Expanding sums to products
  , HExpand(..)
    -- * Transformation of index lists and coercions
  , HTrans(..)
  , hfromI
  , htoI
  ) where

#if !(MIN_VERSION_base(4,8,0))
import Control.Applicative (Applicative)
import Data.Monoid (Monoid)
#endif

import Generics.SOP.BasicFunctors
import Generics.SOP.Constraint

-- * Generalized applicative functor structure

-- ** Generalized 'Control.Applicative.pure'

-- | A generalization of 'Control.Applicative.pure' or
-- 'Control.Monad.return' to higher kinds.
class HPure (h :: (k -> *) -> (l -> *)) where
  -- | Corresponds to 'Control.Applicative.pure' directly.
  --
  -- /Instances:/
  --
  -- @
  -- 'hpure', 'Generics.SOP.NP.pure_NP'  :: 'Generics.SOP.Sing.SListI'  xs  => (forall a. f a) -> 'Generics.SOP.NP.NP'  f xs
  -- 'hpure', 'Generics.SOP.NP.pure_POP' :: 'SListI2' xss => (forall a. f a) -> 'Generics.SOP.NP.POP' f xss
  -- @
  --
  hpure  ::  SListIN h xs => (forall a. f a) -> h f xs

  -- | A variant of 'hpure' that allows passing in a constrained
  -- argument.
  --
  -- Calling @'hcpure' f s@ where @s :: h f xs@ causes @f@ to be
  -- applied at all the types that are contained in @xs@. Therefore,
  -- the constraint @c@ has to be satisfied for all elements of @xs@,
  -- which is what @'AllMap' h c xs@ states.
  --
  -- Morally, 'hpure' is a special case of 'hcpure' where the
  -- constraint is empty. However, it is in the nature of how 'AllMap'
  -- is defined as well as current GHC limitations that it is tricky
  -- to prove to GHC in general that @'AllMap' h c NoConstraint xs@ is
  -- always satisfied. Therefore, we typically define 'hpure'
  -- separately and directly, and make it a member of the class.
  --
  -- /Instances:/
  --
  -- @
  -- 'hcpure', 'Generics.SOP.NP.cpure_NP'  :: ('All'  c xs ) => proxy c -> (forall a. c a => f a) -> 'Generics.SOP.NP.NP'  f xs
  -- 'hcpure', 'Generics.SOP.NP.cpure_POP' :: ('All2' c xss) => proxy c -> (forall a. c a => f a) -> 'Generics.SOP.NP.POP' f xss
  -- @
  --
  hcpure :: (AllN h c xs) => proxy c -> (forall a. c a => f a) -> h f xs

-- ** Generalized 'Control.Applicative.<*>'

-- | Lifted functions.
newtype (f -.-> g) a = Fn { apFn :: f a -> g a }
infixr 1 -.->

-- | Lifted monadic functions.
newtype (f =.=> g) m a = FnM { apFnM :: f a -> m (g a) }
infixr 1 =.=>

-- | Lifted index-changing transformations.
newtype Trans f g a b = Trans { apTrans :: f a -> g b }

-- | Apply a binary lifted function.
apFn_2 :: (f -.-> f' -.-> f'') a -> (f a -> f' a -> f'' a)
apFn_2 f x y = f `apFn` x `apFn` y

-- | Construct a lifted function.
--
-- Same as 'Fn'. Only available for uniformity with the
-- higher-arity versions.
--
fn   :: (f a -> f' a) -> (f -.-> f') a

-- | Construct a binary lifted function.
fn_2 :: (f a -> f' a -> f'' a) -> (f -.-> f' -.-> f'') a

-- | Construct a ternary lifted function.
fn_3 :: (f a -> f' a -> f'' a -> f''' a) -> (f -.-> f' -.-> f'' -.-> f''') a

-- | Construct a quarternary lifted function.
fn_4 :: (f a -> f' a -> f'' a -> f''' a -> f'''' a) -> (f -.-> f' -.-> f'' -.-> f''' -.-> f'''') a

fn   f = Fn $ \x -> f x
fn_2 f = Fn $ \x -> Fn $ \x' -> f x x'
fn_3 f = Fn $ \x -> Fn $ \x' -> Fn $ \x'' -> f x x' x''
fn_4 f = Fn $ \x -> Fn $ \x' -> Fn $ \x'' -> Fn $ \x''' -> f x x' x'' x'''

-- | Maps a structure to the same structure.
type family Same (h :: (k1 -> *) -> (l1 -> *)) :: (k2 -> *) -> (l2 -> *)

-- | Maps a structure containing sums to the corresponding
-- product structure.
type family Prod (h :: (k -> *) -> (l -> *)) :: (k -> *) -> (l -> *)

-- | A generalization of 'Control.Applicative.<*>'.
class (Prod (Prod h) ~ Prod h, HPure (Prod h)) => HAp (h  :: (k -> *) -> (l -> *)) where

  -- | Corresponds to 'Control.Applicative.<*>'.
  --
  -- For products ('Generics.SOP.NP.NP') as well as products of products
  -- ('Generics.SOP.NP.POP'), the correspondence is rather direct. We combine
  -- a structure containing (lifted) functions and a compatible structure
  -- containing corresponding arguments into a compatible structure
  -- containing results.
  --
  -- The same combinator can also be used to combine a product
  -- structure of functions with a sum structure of arguments, which then
  -- results in another sum structure of results. The sum structure
  -- determines which part of the product structure will be used.
  --
  -- /Instances:/
  --
  -- @
  -- 'hap', 'Generics.SOP.NP.ap_NP'  :: 'Generics.SOP.NP.NP'  (f -.-> g) xs  -> 'Generics.SOP.NP.NP'  f xs  -> 'Generics.SOP.NP.NP'  g xs
  -- 'hap', 'Generics.SOP.NS.ap_NS'  :: 'Generics.SOP.NS.NP'  (f -.-> g) xs  -> 'Generics.SOP.NS.NS'  f xs  -> 'Generics.SOP.NS.NS'  g xs
  -- 'hap', 'Generics.SOP.NP.ap_POP' :: 'Generics.SOP.NP.POP' (f -.-> g) xss -> 'Generics.SOP.NP.POP' f xss -> 'Generics.SOP.NP.POP' g xss
  -- 'hap', 'Generics.SOP.NS.ap_SOP' :: 'Generics.SOP.NS.POP' (f -.-> g) xss -> 'Generics.SOP.NS.SOP' f xss -> 'Generics.SOP.NS.SOP' g xss
  -- @
  --
  hap :: SListIN (Prod h) xs => Prod h (f -.-> g) xs -> h f xs -> h g xs

-- ** Derived functions

-- | A generalized form of 'Control.Applicative.liftA',
-- which in turn is a generalized 'map'.
--
-- Takes a lifted function and applies it to every element of
-- a structure while preserving its shape.
--
-- /Specification:/
--
-- @
-- 'hliftA' f xs = 'hpure' ('fn' f) \` 'hap' \` xs
-- @
--
-- /Instances:/
--
-- @
-- 'hliftA', 'Generics.SOP.NP.liftA_NP'  :: 'Generics.SOP.Sing.SListI'  xs  => (forall a. f a -> f' a) -> 'Generics.SOP.NP.NP'  f xs  -> 'Generics.SOP.NP.NP'  f' xs
-- 'hliftA', 'Generics.SOP.NS.liftA_NS'  :: 'Generics.SOP.Sing.SListI'  xs  => (forall a. f a -> f' a) -> 'Generics.SOP.NS.NS'  f xs  -> 'Generics.SOP.NS.NS'  f' xs
-- 'hliftA', 'Generics.SOP.NP.liftA_POP' :: 'SListI2' xss => (forall a. f a -> f' a) -> 'Generics.SOP.NP.POP' f xss -> 'Generics.SOP.NP.POP' f' xss
-- 'hliftA', 'Generics.SOP.NS.liftA_SOP' :: 'SListI2' xss => (forall a. f a -> f' a) -> 'Generics.SOP.NS.SOP' f xss -> 'Generics.SOP.NS.SOP' f' xss
-- @
--
hliftA  :: (SListIN (Prod h) xs, HAp h)               => (forall a. f a -> f' a)                                                   -> h f   xs -> h f'   xs

-- | A generalized form of 'Control.Applicative.liftA2',
-- which in turn is a generalized 'zipWith'.
--
-- Takes a lifted binary function and uses it to combine two
-- structures of equal shape into a single structure.
--
-- It either takes two product structures to a product structure,
-- or one product and one sum structure to a sum structure.
--
-- /Specification:/
--
-- @
-- 'hliftA2' f xs ys = 'hpure' ('fn_2' f) \` 'hap' \` xs \` 'hap' \` ys
-- @
--
-- /Instances:/
--
-- @
-- 'hliftA2', 'Generics.SOP.NP.liftA2_NP'  :: 'Generics.SOP.Sing.SListI'  xs  => (forall a. f a -> f' a -> f'' a) -> 'Generics.SOP.NP.NP'  f xs  -> 'Generics.SOP.NP.NP'  f' xs  -> 'Generics.SOP.NP.NP'  f'' xs
-- 'hliftA2', 'Generics.SOP.NS.liftA2_NS'  :: 'Generics.SOP.Sing.SListI'  xs  => (forall a. f a -> f' a -> f'' a) -> 'Generics.SOP.NP.NP'  f xs  -> 'Generics.SOP.NS.NS'  f' xs  -> 'Generics.SOP.NS.NS'  f'' xs
-- 'hliftA2', 'Generics.SOP.NP.liftA2_POP' :: 'SListI2' xss => (forall a. f a -> f' a -> f'' a) -> 'Generics.SOP.NP.POP' f xss -> 'Generics.SOP.NP.POP' f' xss -> 'Generics.SOP.NP.POP' f'' xss
-- 'hliftA2', 'Generics.SOP.NS.liftA2_SOP' :: 'SListI2' xss => (forall a. f a -> f' a -> f'' a) -> 'Generics.SOP.NP.POP' f xss -> 'Generics.SOP.NS.SOP' f' xss -> 'Generics.SOP.NS.SOP' f'' xss
-- @
--
hliftA2 :: (SListIN (Prod h) xs, HAp h, HAp (Prod h)) => (forall a. f a -> f' a -> f'' a)           -> Prod h f xs                 -> h f'  xs -> h f''  xs

-- | A generalized form of 'Control.Applicative.liftA3',
-- which in turn is a generalized 'zipWith3'.
--
-- Takes a lifted ternary function and uses it to combine three
-- structures of equal shape into a single structure.
--
-- It either takes three product structures to a product structure,
-- or two product structures and one sum structure to a sum structure.
--
-- /Specification:/
--
-- @
-- 'hliftA3' f xs ys zs = 'hpure' ('fn_3' f) \` 'hap' \` xs \` 'hap' \` ys \` 'hap' \` zs
-- @
--
-- /Instances:/
--
-- @
-- 'hliftA3', 'Generics.SOP.NP.liftA3_NP'  :: 'Generics.SOP.Sing.SListI'  xs  => (forall a. f a -> f' a -> f'' a -> f''' a) -> 'Generics.SOP.NP.NP'  f xs  -> 'Generics.SOP.NP.NP'  f' xs  -> 'Generics.SOP.NP.NP'  f'' xs  -> 'Generics.SOP.NP.NP'  f''' xs
-- 'hliftA3', 'Generics.SOP.NS.liftA3_NS'  :: 'Generics.SOP.Sing.SListI'  xs  => (forall a. f a -> f' a -> f'' a -> f''' a) -> 'Generics.SOP.NP.NP'  f xs  -> 'Generics.SOP.NP.NP'  f' xs  -> 'Generics.SOP.NS.NS'  f'' xs  -> 'Generics.SOP.NS.NS'  f''' xs
-- 'hliftA3', 'Generics.SOP.NP.liftA3_POP' :: 'SListI2' xss => (forall a. f a -> f' a -> f'' a -> f''' a) -> 'Generics.SOP.NP.POP' f xss -> 'Generics.SOP.NP.POP' f' xss -> 'Generics.SOP.NP.POP' f'' xss -> 'Generics.SOP.NP.POP' f''' xs
-- 'hliftA3', 'Generics.SOP.NS.liftA3_SOP' :: 'SListI2' xss => (forall a. f a -> f' a -> f'' a -> f''' a) -> 'Generics.SOP.NP.POP' f xss -> 'Generics.SOP.NP.POP' f' xss -> 'Generics.SOP.NS.SOP' f'' xss -> 'Generics.SOP.NP.SOP' f''' xs
-- @
--
hliftA3 :: (SListIN (Prod h) xs, HAp h, HAp (Prod h)) => (forall a. f a -> f' a -> f'' a -> f''' a) -> Prod h f xs -> Prod h f' xs -> h f'' xs -> h f''' xs

hliftA  f xs       = hpure (fn   f) `hap` xs
hliftA2 f xs ys    = hpure (fn_2 f) `hap` xs `hap` ys
hliftA3 f xs ys zs = hpure (fn_3 f) `hap` xs `hap` ys `hap` zs

-- | Another name for 'hliftA'.
--
-- @since 0.2
--
hmap      :: (SListIN (Prod h) xs, HAp h)               => (forall a. f a -> f' a)                                                   -> h f   xs -> h f'   xs

-- | Another name for 'hliftA2'.
--
-- @since 0.2
--
hzipWith  :: (SListIN (Prod h) xs, HAp h, HAp (Prod h)) => (forall a. f a -> f' a -> f'' a)           -> Prod h f xs                 -> h f'  xs -> h f''  xs

-- | Another name for 'hliftA3'.
--
-- @since 0.2
--
hzipWith3 :: (SListIN (Prod h) xs, HAp h, HAp (Prod h)) => (forall a. f a -> f' a -> f'' a -> f''' a) -> Prod h f xs -> Prod h f' xs -> h f'' xs -> h f''' xs

hmap      = hliftA
hzipWith  = hliftA2
hzipWith3 = hliftA3

-- | Variant of 'hliftA' that takes a constrained function.
--
-- /Specification:/
--
-- @
-- 'hcliftA' p f xs = 'hcpure' p ('fn' f) \` 'hap' \` xs
-- @
--
hcliftA  :: (SListIN (Prod h) xs, AllN (Prod h) c xs, HAp h)               => proxy c -> (forall a. c a => f a -> f' a)                                                   -> h f   xs -> h f'   xs

-- | Variant of 'hcliftA2' that takes a constrained function.
--
-- /Specification:/
--
-- @
-- 'hcliftA2' p f xs ys = 'hcpure' p ('fn_2' f) \` 'hap' \` xs \` 'hap' \` ys
-- @
--
hcliftA2 :: (SListIN (Prod h) xs, AllN (Prod h) c xs, HAp h, HAp (Prod h)) => proxy c -> (forall a. c a => f a -> f' a -> f'' a)           -> Prod h f xs                 -> h f'  xs -> h f''  xs

-- | Variant of 'hcliftA3' that takes a constrained function.
--
-- /Specification:/
--
-- @
-- 'hcliftA3' p f xs ys zs = 'hcpure' p ('fn_3' f) \` 'hap' \` xs \` 'hap' \` ys \` 'hap' \` zs
-- @
--
hcliftA3 :: (SListIN (Prod h) xs, AllN (Prod h) c xs, HAp h, HAp (Prod h)) => proxy c -> (forall a. c a => f a -> f' a -> f'' a -> f''' a) -> Prod h f xs -> Prod h f' xs -> h f'' xs -> h f''' xs

hcliftA  p f xs       = hcpure p (fn   f) `hap` xs
{-# INLINE hcliftA #-}
hcliftA2 p f xs ys    = hcpure p (fn_2 f) `hap` xs `hap` ys
{-# INLINE hcliftA2 #-}
hcliftA3 p f xs ys zs = hcpure p (fn_3 f) `hap` xs `hap` ys `hap` zs
{-# INLINE hcliftA3 #-}

-- | Another name for 'hcliftA'.
--
-- @since 0.2
--
hcmap      :: (SListIN (Prod h) xs, AllN (Prod h) c xs, HAp h)               => proxy c -> (forall a. c a => f a -> f' a)                                                   -> h f   xs -> h f'   xs

-- | Another name for 'hcliftA2'.
--
-- @since 0.2
--
hczipWith  :: (SListIN (Prod h) xs, AllN (Prod h) c xs, HAp h, HAp (Prod h)) => proxy c -> (forall a. c a => f a -> f' a -> f'' a)           -> Prod h f xs                 -> h f'  xs -> h f''  xs

-- | Another name for 'hcliftA3'.
--
-- @since 0.2
--
hczipWith3 :: (SListIN (Prod h) xs, AllN (Prod h) c xs, HAp h, HAp (Prod h)) => proxy c -> (forall a. c a => f a -> f' a -> f'' a -> f''' a) -> Prod h f xs -> Prod h f' xs -> h f'' xs -> h f''' xs

hcmap      = hcliftA
{-# INLINE hcmap #-}
hczipWith  = hcliftA2
{-# INLINE hczipWith #-}
hczipWith3 = hcliftA3
{-# INLINE hczipWith3 #-}

-- * Collapsing homogeneous structures

-- | Maps products to lists, and sums to identities.
type family CollapseTo (h :: (k -> *) -> (l -> *)) (x :: *) :: *

-- | A class for collapsing a heterogeneous structure into
-- a homogeneous one.
class HCollapse (h :: (k -> *) -> (l -> *)) where

  -- | Collapse a heterogeneous structure with homogeneous elements
  -- into a homogeneous structure.
  --
  -- If a heterogeneous structure is instantiated to the constant
  -- functor 'K', then it is in fact homogeneous. This function
  -- maps such a value to a simpler Haskell datatype reflecting that.
  -- An @'NS' ('K' a)@ contains a single @a@, and an @'NP' ('K' a)@ contains
  -- a list of @a@s.
  --
  -- /Instances:/
  --
  -- @
  -- 'hcollapse', 'Generics.SOP.NP.collapse_NP'  :: 'Generics.SOP.NP.NP'  ('K' a) xs  ->  [a]
  -- 'hcollapse', 'Generics.SOP.NS.collapse_NS'  :: 'Generics.SOP.NS.NS'  ('K' a) xs  ->   a
  -- 'hcollapse', 'Generics.SOP.NP.collapse_POP' :: 'Generics.SOP.NP.POP' ('K' a) xss -> [[a]]
  -- 'hcollapse', 'Generics.SOP.NS.collapse_SOP' :: 'Generics.SOP.NP.SOP' ('K' a) xss ->  [a]
  -- @
  --
  hcollapse :: SListIN h xs => h (K a) xs -> CollapseTo h a

-- | A generalization of 'Data.Foldable.traverse_' or 'Data.Foldable.foldMap'.
--
-- @since 0.3.2.0
--
class HTraverse_ (h :: (k -> *) -> (l -> *)) where

  -- | Corresponds to 'Data.Foldable.traverse_'.
  --
  -- /Instances:/
  --
  -- @
  -- 'hctraverse_', 'Generics.SOP.NP.ctraverse__NP'  :: ('All'  c xs , 'Applicative' g) => proxy c -> (forall a. c a => f a -> g ()) -> 'Generics.SOP.NP.NP'  f xs  -> g ()
  -- 'hctraverse_', 'Generics.SOP.NS.ctraverse__NS'  :: ('All2' c xs , 'Applicative' g) => proxy c -> (forall a. c a => f a -> g ()) -> 'Generics.SOP.NS.NS'  f xs  -> g ()
  -- 'hctraverse_', 'Generics.SOP.NP.ctraverse__POP' :: ('All'  c xss, 'Applicative' g) => proxy c -> (forall a. c a => f a -> g ()) -> 'Generics.SOP.NP.POP' f xss -> g ()
  -- 'hctraverse_', 'Generics.SOP.NS.ctraverse__SOP' :: ('All2' c xss, 'Applicative' g) => proxy c -> (forall a. c a => f a -> g ()) -> 'Generics.SOP.NS.SOP' f xss -> g ()
  -- @
  --
  -- @since 0.3.2.0
  --
  hctraverse_ :: (AllN h c xs, Applicative g) => proxy c -> (forall a. c a => f a -> g ()) -> h f xs -> g ()

  -- | Unconstrained version of 'hctraverse_'.
  --
  -- /Instances:/
  --
  -- @
  -- 'traverse_', 'Generics.SOP.NP.traverse__NP'  :: ('SListI'  xs , 'Applicative g') => (forall a. f a -> g ()) -> 'Generics.SOP.NP.NP'  f xs  -> g ()
  -- 'traverse_', 'Generics.SOP.NS.traverse__NS'  :: ('SListI'  xs , 'Applicative g') => (forall a. f a -> g ()) -> 'Generics.SOP.NS.NS'  f xs  -> g ()
  -- 'traverse_', 'Generics.SOP.NP.traverse__POP' :: ('SListI2' xss, 'Applicative g') => (forall a. f a -> g ()) -> 'Generics.SOP.NP.POP' f xss -> g ()
  -- 'traverse_', 'Generics.SOP.NS.traverse__SOP' :: ('SListI2' xss, 'Applicative g') => (forall a. f a -> g ()) -> 'Generics.SOP.NS.SOP' f xss -> g ()
  -- @
  --
  -- @since 0.3.2.0
  --
  htraverse_ :: (SListIN h xs, Applicative g) => (forall a. f a -> g ()) -> h f xs -> g ()

-- | Flipped version of 'hctraverse_'.
--
-- @since 0.3.2.0
--
hcfor_ :: (HTraverse_ h, AllN h c xs, Applicative g) => proxy c -> h f xs -> (forall a. c a => f a -> g ()) -> g ()
hcfor_ p xs f = hctraverse_ p f xs

-- | Special case of 'hctraverse_'.
--
-- @since 0.3.2.0
--
hcfoldMap :: (HTraverse_ h, AllN h c xs, Monoid m) => proxy c -> (forall a. c a => f a -> m) -> h f xs -> m
hcfoldMap p f = unK . hctraverse_ p (K . f)

-- * Sequencing effects

-- | A generalization of 'Data.Traversable.sequenceA'.
class HAp h => HSequence (h :: (k -> *) -> (l -> *)) where

  -- | Corresponds to 'Data.Traversable.sequenceA'.
  --
  -- Lifts an applicative functor out of a structure.
  --
  -- /Instances:/
  --
  -- @
  -- 'hsequence'', 'Generics.SOP.NP.sequence'_NP'  :: ('Generics.SOP.Sing.SListI'  xs , 'Applicative' f) => 'Generics.SOP.NP.NP'  (f ':.:' g) xs  -> f ('Generics.SOP.NP.NP'  g xs )
  -- 'hsequence'', 'Generics.SOP.NS.sequence'_NS'  :: ('Generics.SOP.Sing.SListI'  xs , 'Applicative' f) => 'Generics.SOP.NS.NS'  (f ':.:' g) xs  -> f ('Generics.SOP.NS.NS'  g xs )
  -- 'hsequence'', 'Generics.SOP.NP.sequence'_POP' :: ('SListI2' xss, 'Applicative' f) => 'Generics.SOP.NP.POP' (f ':.:' g) xss -> f ('Generics.SOP.NP.POP' g xss)
  -- 'hsequence'', 'Generics.SOP.NS.sequence'_SOP' :: ('SListI2' xss, 'Applicative' f) => 'Generics.SOP.NS.SOP' (f ':.:' g) xss -> f ('Generics.SOP.NS.SOP' g xss)
  -- @
  --
  hsequence' :: (SListIN h xs, Applicative f) => h (f :.: g) xs -> f (h g xs)


  -- | Corresponds to 'Data.Traversable.traverse'.
  --
  -- /Instances:/
  --
  -- @
  -- 'hctraverse'', 'Generics.SOP.NP.ctraverse'_NP'  :: ('All'  c xs , 'Applicative' g) => proxy c -> (forall a. c a => f a -> g (f' a)) -> 'Generics.SOP.NP.NP'  f xs  -> g ('Generics.SOP.NP.NP'  f' xs )
  -- 'hctraverse'', 'Generics.SOP.NS.ctraverse'_NS'  :: ('All2' c xs , 'Applicative' g) => proxy c -> (forall a. c a => f a -> g (f' a)) -> 'Generics.SOP.NS.NS'  f xs  -> g ('Generics.SOP.NS.NS'  f' xs )
  -- 'hctraverse'', 'Generics.SOP.NP.ctraverse'_POP' :: ('All'  c xss, 'Applicative' g) => proxy c -> (forall a. c a => f a -> g (f' a)) -> 'Generics.SOP.NP.POP' f xss -> g ('Generics.SOP.NP.POP' f' xss)
  -- 'hctraverse'', 'Generics.SOP.NS.ctraverse'_SOP' :: ('All2' c xss, 'Applicative' g) => proxy c -> (forall a. c a => f a -> g (f' a)) -> 'Generics.SOP.NS.SOP' f xss -> g ('Generics.SOP.NS.SOP' f' xss)
  -- @
  --
  -- @since 0.3.2.0
  --
  hctraverse' :: (AllN h c xs, Applicative g) => proxy c -> (forall a. c a => f a -> g (f' a)) -> h f xs -> g (h f' xs)

  -- | Unconstrained variant of `htraverse'`.
  --
  -- /Instances:/
  --
  -- @
  -- 'htraverse'', 'Generics.SOP.NP.traverse'_NP'  :: ('SListI'  xs , 'Applicative' g) => (forall a. c a => f a -> g (f' a)) -> 'Generics.SOP.NP.NP'  f xs  -> g ('Generics.SOP.NP.NP'  f' xs )
  -- 'htraverse'', 'Generics.SOP.NS.traverse'_NS'  :: ('SListI2' xs , 'Applicative' g) => (forall a. c a => f a -> g (f' a)) -> 'Generics.SOP.NS.NS'  f xs  -> g ('Generics.SOP.NS.NS'  f' xs )
  -- 'htraverse'', 'Generics.SOP.NP.traverse'_POP' :: ('SListI'  xss, 'Applicative' g) => (forall a. c a => f a -> g (f' a)) -> 'Generics.SOP.NP.POP' f xss -> g ('Generics.SOP.NP.POP' f' xss)
  -- 'htraverse'', 'Generics.SOP.NS.traverse'_SOP' :: ('SListI2' xss, 'Applicative' g) => (forall a. c a => f a -> g (f' a)) -> 'Generics.SOP.NS.SOP' f xss -> g ('Generics.SOP.NS.SOP' f' xss)
  -- @
  --
  -- @since 0.3.2.0
  --
  htraverse' :: (SListIN h xs, Applicative g) => (forall a. f a -> g (f' a)) -> h f xs -> g (h f' xs)

-- ** Derived functions

-- | Special case of 'hctraverse'' where @f' = 'I'@.
--
-- @since 0.3.2.0
--
hctraverse :: (HSequence h, AllN h c xs, Applicative g) => proxy c -> (forall a. c a => f a -> g a) -> h f xs -> g (h I xs)
hctraverse p f = hctraverse' p (fmap I . f)

-- | Flipped version of 'hctraverse'.
--
-- @since 0.3.2.0
--
hcfor :: (HSequence h, AllN h c xs, Applicative g) => proxy c -> h f xs -> (forall a. c a => f a -> g a) -> g (h I xs)
hcfor p xs f = hctraverse p f xs

-- | Special case of 'hsequence'' where @g = 'I'@.
hsequence :: (SListIN h xs, SListIN (Prod h) xs, HSequence h) => Applicative f => h f xs -> f (h I xs)
hsequence = hsequence' . hliftA (Comp . fmap I)

-- | Special case of 'hsequence'' where @g = 'K' a@.
hsequenceK ::  (SListIN h xs, SListIN (Prod h) xs, Applicative f, HSequence h) => h (K (f a)) xs -> f (h (K a) xs)
hsequenceK = hsequence' . hliftA (Comp . fmap K . unK)

-- * Indexing into sums

-- | A class for determining which choice in a sum-like structure
-- a value represents.
--
class HIndex (h :: (k -> *) -> (l -> *)) where

  -- | If 'h' is a sum-like structure representing a choice
  -- between @n@ different options, and @x@ is a value of
  -- type @h f xs@, then @'hindex' x@ returns a number between
  -- @0@ and @n - 1@ representing the index of the choice
  -- made by @x@.
  --
  -- /Instances:/
  --
  -- @
  -- 'hindex', 'Generics.SOP.NS.index_NS'  :: 'Generics.SOP.NS.NS'  f xs -> Int
  -- 'hindex', 'Generics.SOP.NS.index_SOP' :: 'Generics.SOP.NS.SOP' f xs -> Int
  -- @
  --
  -- /Examples:/
  --
  -- >>> hindex (S (S (Z (I False))) :: NS I '[Char, Int, Bool])
  -- 2
  -- >>> hindex (Z (K ()) :: NS (K ()) '[Char, Int, Bool])
  -- 0
  -- >>> hindex (SOP (S (Z (I True :* I 'x' :* Nil))) :: SOP I '[ '[Int], '[Bool, Char] ])
  -- 1
  --
  -- @since 0.2.4.0
  --
  hindex :: (SListIN h xs) => h f xs -> Int

-- * Applying all injections

-- | Maps a structure containing products to the corresponding
-- sum structure.
--
-- @since 0.2.4.0
--
type family UnProd (h :: (k -> *) -> (l -> *)) :: (k -> *) -> (l -> *)

-- | A class for applying all injections corresponding to a sum-like
-- structure to a table containing suitable arguments.
--
class (UnProd (Prod h) ~ h) => HApInjs (h :: (k -> *) -> (l -> *)) where

  -- | For a given table (product-like structure), produce a list where
  -- each element corresponds to the application of an injection function
  -- into the corresponding sum-like structure.
  --
  -- /Instances:/
  --
  -- @
  -- 'hapInjs', 'Generics.SOP.NS.apInjs_NP'  :: 'Generics.SOP.Sing.SListI'  xs  => 'Generics.SOP.NP.NP'  f xs -> ['Generics.SOP.NS.NS'  f xs ]
  -- 'hapInjs', 'Generics.SOP.NS.apInjs_SOP' :: 'SListI2' xss => 'Generics.SOP.NP.POP' f xs -> ['Generics.SOP.NS.SOP' f xss]
  -- @
  --
  -- /Examples:/
  --
  -- >>> hapInjs (I 'x' :* I True :* I 2 :* Nil) :: [NS I '[Char, Bool, Int]]
  -- [Z (I 'x'),S (Z (I True)),S (S (Z (I 2)))]
  --
  -- >>> hapInjs (POP ((I 'x' :* Nil) :* (I True :* I 2 :* Nil) :* Nil)) :: [SOP I '[ '[Char], '[Bool, Int]]]
  -- [SOP (Z (I 'x' :* Nil)),SOP (S (Z (I True :* I 2 :* Nil)))]
  --
  -- Unfortunately the type-signatures are required in GHC-7.10 and older.
  --
  -- @since 0.2.4.0
  --
  hapInjs :: (SListIN h xs) => Prod h f xs -> [h f xs]

-- * Expanding sums to products

-- | A class for expanding sum structures into corresponding product
-- structures, filling in the slots not targeted by the sum with
-- default values.
--
-- @since 0.2.5.0
--
class HExpand (h :: (k -> *) -> (l -> *)) where

  -- | Expand a given sum structure into a corresponding product
  -- structure by placing the value contained in the sum into the
  -- corresponding position in the product, and using the given
  -- default value for all other positions.
  --
  -- /Instances:/
  --
  -- @
  -- 'hexpand', 'Generics.SOP.NS.expand_NS'  :: 'Generics.SOP.Sing.SListI' xs  => (forall x . f x) -> 'Generics.SOP.NS.NS'  f xs  -> 'Generics.SOP.NS.NP'  f xs
  -- 'hexpand', 'Generics.SOP.NS.expand_SOP' :: 'SListI2' xss => (forall x . f x) -> 'Generics.SOP.NS.SOP' f xss -> 'Generics.SOP.NP.POP' f xss
  -- @
  --
  -- /Examples:/
  --
  -- >>> hexpand Nothing (S (Z (Just 3))) :: NP Maybe '[Char, Int, Bool]
  -- Nothing :* Just 3 :* Nothing :* Nil
  -- >>> hexpand [] (SOP (S (Z ([1,2] :* "xyz" :* Nil)))) :: POP [] '[ '[Bool], '[Int, Char] ]
  -- POP (([] :* Nil) :* ([1,2] :* "xyz" :* Nil) :* Nil)
  --
  -- @since 0.2.5.0
  --
  hexpand :: (SListIN (Prod h) xs) => (forall x . f x) -> h f xs -> Prod h f xs

  -- | Variant of 'hexpand' that allows passing a constrained default.
  --
  -- /Instances:/
  --
  -- @
  -- 'hcexpand', 'Generics.SOP.NS.cexpand_NS'  :: 'All'  c xs  => proxy c -> (forall x . c x => f x) -> 'Generics.SOP.NS.NS'  f xs  -> 'Generics.SOP.NP.NP'  f xs
  -- 'hcexpand', 'Generics.SOP.NS.cexpand_SOP' :: 'All2' c xss => proxy c -> (forall x . c x => f x) -> 'Generics.SOP.NS.SOP' f xss -> 'Generics.SOP.NP.POP' f xss
  -- @
  --
  -- /Examples:/
  --
  -- >>> hcexpand (Proxy :: Proxy Bounded) (I minBound) (S (Z (I 20))) :: NP I '[Bool, Int, Ordering]
  -- I False :* I 20 :* I LT :* Nil
  -- >>> hcexpand (Proxy :: Proxy Num) (I 0) (SOP (S (Z (I 1 :* I 2 :* Nil)))) :: POP I '[ '[Double], '[Int, Int] ]
  -- POP ((I 0.0 :* Nil) :* (I 1 :* I 2 :* Nil) :* Nil)
  --
  -- @since 0.2.5.0
  --
  hcexpand :: (AllN (Prod h) c xs) => proxy c -> (forall x . c x => f x) -> h f xs -> Prod h f xs

-- | A class for transforming structures into related structures with
-- a different index list, as long as the index lists have the same shape
-- and the elements and interpretation functions are suitably related.
--
-- @since 0.3.1.0
--
class (Same h1 ~ h2, Same h2 ~ h1) => HTrans (h1 :: (k1 -> *) -> (l1 -> *)) (h2 :: (k2 -> *) -> (l2 -> *)) where

  -- | Transform a structure into a related structure given a conversion
  -- function for the elements.
  --
  -- @since 0.3.1.0
  --
  htrans ::
       AllZipN (Prod h1) c xs ys
    => proxy c
    -> (forall x y . c x y => f x -> g y)
    -> h1 f xs -> h2 g ys

  -- | Coerce a structure into a representationally equal structure.
  --
  -- /Examples:/
  --
  -- >>> hcoerce (I (Just LT) :* I (Just 'x') :* I (Just True) :* Nil) :: NP Maybe '[Ordering, Char, Bool]
  -- Just LT :* Just 'x' :* Just True :* Nil
  -- >>> hcoerce (SOP (Z (K True :* K False :* Nil))) :: SOP I '[ '[Bool, Bool], '[Bool] ]
  -- SOP (Z (I True :* I False :* Nil))
  --
  -- @since 0.3.1.0
  hcoerce ::
       (AllZipN (Prod h1) (LiftedCoercible f g) xs ys, HTrans h1 h2)
    => h1 f xs -> h2 g ys

-- | Specialization of 'hcoerce'.
--
-- @since 0.3.1.0
--
hfromI ::
       (AllZipN (Prod h1) (LiftedCoercible I f) xs ys, HTrans h1 h2)
    => h1 I xs -> h2 f ys
hfromI = hcoerce

-- | Specialization of 'hcoerce'.
--
-- @since 0.3.1.0
--
htoI ::
       (AllZipN (Prod h1) (LiftedCoercible f I) xs ys, HTrans h1 h2)
    => h1 f xs -> h2 I ys
htoI = hcoerce

-- $setup
-- >>> import Generics.SOP
