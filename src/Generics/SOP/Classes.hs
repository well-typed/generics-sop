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
module Generics.SOP.Classes where

#if !(MIN_VERSION_base(4,8,0))
import Control.Applicative (Applicative)
#endif

import Generics.SOP.BasicFunctors
import Generics.SOP.Constraint

-- | A generalization of 'Control.Applicative.pure' or
-- 'Control.Monad.return' to higher kinds.
class HPure (h :: (k -> *) -> (l -> *)) where
  -- | Corresponds to 'Control.Applicative.pure' directly.
  --
  -- /Instances:/
  --
  -- @
  -- 'hpure', 'Generics.SOP.NP.pure_NP'  :: 'SListI'  xs  => (forall a. f a) -> 'Generics.SOP.NP.NP'  f xs
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

{-------------------------------------------------------------------------------
  Application
-------------------------------------------------------------------------------}

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
  hap :: Prod h (f -.-> g) xs -> h f xs -> h g xs

{-------------------------------------------------------------------------------
  Derived from application
-------------------------------------------------------------------------------}

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
-- 'hliftA', 'Generics.SOP.NP.liftA_NP'  :: 'SListI'  xs  => (forall a. f a -> f' a) -> 'Generics.SOP.NP.NP'  f xs  -> 'Generics.SOP.NP.NP'  f' xs
-- 'hliftA', 'Generics.SOP.NS.liftA_NS'  :: 'SListI'  xs  => (forall a. f a -> f' a) -> 'Generics.SOP.NS.NS'  f xs  -> 'Generics.SOP.NS.NS'  f' xs
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
-- 'hliftA2', 'Generics.SOP.NP.liftA2_NP'  :: 'SListI'  xs  => (forall a. f a -> f' a -> f'' a) -> 'Generics.SOP.NP.NP'  f xs  -> 'Generics.SOP.NP.NP'  f' xs  -> 'Generics.SOP.NP.NP'  f'' xs
-- 'hliftA2', 'Generics.SOP.NS.liftA2_NS'  :: 'SListI'  xs  => (forall a. f a -> f' a -> f'' a) -> 'Generics.SOP.NP.NP'  f xs  -> 'Generics.SOP.NS.NS'  f' xs  -> 'Generics.SOP.NS.NS'  f'' xs
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
-- 'hliftA3', 'Generics.SOP.NP.liftA3_NP'  :: 'SListI'  xs  => (forall a. f a -> f' a -> f'' a -> f''' a) -> 'Generics.SOP.NP.NP'  f xs  -> 'Generics.SOP.NP.NP'  f' xs  -> 'Generics.SOP.NP.NP'  f'' xs  -> 'Generics.SOP.NP.NP'  f''' xs
-- 'hliftA3', 'Generics.SOP.NS.liftA3_NS'  :: 'SListI'  xs  => (forall a. f a -> f' a -> f'' a -> f''' a) -> 'Generics.SOP.NP.NP'  f xs  -> 'Generics.SOP.NP.NP'  f' xs  -> 'Generics.SOP.NS.NS'  f'' xs  -> 'Generics.SOP.NS.NS'  f''' xs
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
hcliftA  :: (AllN (Prod h) c xs, HAp h)               => proxy c -> (forall a. c a => f a -> f' a)                                                   -> h f   xs -> h f'   xs

-- | Variant of 'hcliftA2' that takes a constrained function.
--
-- /Specification:/
--
-- @
-- 'hcliftA2' p f xs ys = 'hcpure' p ('fn_2' f) \` 'hap' \` xs \` 'hap' \` ys
-- @
--
hcliftA2 :: (AllN (Prod h) c xs, HAp h, HAp (Prod h)) => proxy c -> (forall a. c a => f a -> f' a -> f'' a)           -> Prod h f xs                 -> h f'  xs -> h f''  xs

-- | Variant of 'hcliftA3' that takes a constrained function.
--
-- /Specification:/
--
-- @
-- 'hcliftA3' p f xs ys zs = 'hcpure' p ('fn_3' f) \` 'hap' \` xs \` 'hap' \` ys \` 'hap' \` zs
-- @
--
hcliftA3 :: (AllN (Prod h) c xs, HAp h, HAp (Prod h)) => proxy c -> (forall a. c a => f a -> f' a -> f'' a -> f''' a) -> Prod h f xs -> Prod h f' xs -> h f'' xs -> h f''' xs

hcliftA  p f xs       = hcpure p (fn   f) `hap` xs
hcliftA2 p f xs ys    = hcpure p (fn_2 f) `hap` xs `hap` ys
hcliftA3 p f xs ys zs = hcpure p (fn_3 f) `hap` xs `hap` ys `hap` zs

-- | Another name for 'hcliftA'.
--
-- @since 0.2
--
hcmap      :: (AllN (Prod h) c xs, HAp h)               => proxy c -> (forall a. c a => f a -> f' a)                                                   -> h f   xs -> h f'   xs

-- | Another name for 'hcliftA2'.
--
-- @since 0.2
--
hczipWith  :: (AllN (Prod h) c xs, HAp h, HAp (Prod h)) => proxy c -> (forall a. c a => f a -> f' a -> f'' a)           -> Prod h f xs                 -> h f'  xs -> h f''  xs

-- | Another name for 'hcliftA3'.
--
-- @since 0.2
--
hczipWith3 :: (AllN (Prod h) c xs, HAp h, HAp (Prod h)) => proxy c -> (forall a. c a => f a -> f' a -> f'' a -> f''' a) -> Prod h f xs -> Prod h f' xs -> h f'' xs -> h f''' xs

hcmap      = hcliftA
hczipWith  = hcliftA2
hczipWith3 = hcliftA3

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

-- | A generalization of 'Data.Traversable.sequenceA'.
class HAp h => HSequence (h :: (k -> *) -> (l -> *)) where

  -- | Corresponds to 'Data.Traversable.sequenceA'.
  --
  -- Lifts an applicative functor out of a structure.
  --
  -- /Instances:/
  --
  -- @
  -- 'hsequence'', 'Generics.SOP.NP.sequence'_NP'  :: ('SListI'  xs , 'Applicative' f) => 'Generics.SOP.NP.NP'  (f ':.:' g) xs  -> f ('Generics.SOP.NP.NP'  g xs )
  -- 'hsequence'', 'Generics.SOP.NS.sequence'_NS'  :: ('SListI'  xs , 'Applicative' f) => 'Generics.SOP.NS.NS'  (f ':.:' g) xs  -> f ('Generics.SOP.NS.NS'  g xs )
  -- 'hsequence'', 'Generics.SOP.NP.sequence'_POP' :: ('SListI2' xss, 'Applicative' f) => 'Generics.SOP.NP.POP' (f ':.:' g) xss -> f ('Generics.SOP.NP.POP' g xss)
  -- 'hsequence'', 'Generics.SOP.NS.sequence'_SOP' :: ('SListI2' xss, 'Applicative' f) => 'Generics.SOP.NS.SOP' (f ':.:' g) xss -> f ('Generics.SOP.NS.SOP' g xss)
  -- @
  --
  hsequence' :: (SListIN h xs, Applicative f) => h (f :.: g) xs -> f (h g xs)

-- | Special case of 'hsequence'' where @g = 'I'@.
hsequence :: (SListIN h xs, SListIN (Prod h) xs, HSequence h) => Applicative f => h f xs -> f (h I xs)
hsequence = hsequence' . hliftA (Comp . fmap I)

-- | Special case of 'hsequence'' where @g = 'K' a@.
hsequenceK ::  (SListIN h xs, SListIN (Prod h) xs, Applicative f, HSequence h) => h (K (f a)) xs -> f (h (K a) xs)
hsequenceK = hsequence' . hliftA (Comp . fmap K . unK)
