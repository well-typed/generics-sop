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

import Control.Applicative (Applicative)
import Data.Proxy (Proxy)

import Generics.SOP.BasicFunctors
import Generics.SOP.Constraint
import Generics.SOP.Sing

-- | A generalization of 'Control.Applicative.pure' or
-- 'Control.Monad.return' to higher kinds.
class HPure (h :: (k -> *) -> (l -> *)) where
  -- | Corresponds to 'Control.Applicative.pure' directly.
  --
  -- /Instances:/
  --
  -- @
  -- 'hpure', 'Generics.SOP.NP.pure_NP'  :: 'SingI' xs  => (forall a. f a) -> 'Generics.SOP.NP.NP'  f xs
  -- 'hpure', 'Generics.SOP.NP.pure_POP' :: 'SingI' xss => (forall a. f a) -> 'Generics.SOP.NP.POP' f xss
  -- @
  --
  hpure  ::  SingI xs => (forall a. f a) -> h f xs

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
  -- 'hcpure', 'Generics.SOP.NP.cpure_NP'  :: ('SingI' xs,  'All'  c xs ) => 'Proxy' c -> (forall a. c a => f a) -> 'Generics.SOP.NP.NP'  f xs
  -- 'hcpure', 'Generics.SOP.NP.cpure_POP' :: ('SingI' xsx, 'All2' c xsx) => 'Proxy' c -> (forall a. c a => f a) -> 'Generics.SOP.NP.POP' f xs
  -- @
  --
  hcpure :: (SingI xs, AllMap h c xs) => Proxy c -> (forall a. c a => f a) -> h f xs

{-------------------------------------------------------------------------------
  Application
-------------------------------------------------------------------------------}

newtype (f -.-> g) a = Fn { apFn :: f a -> g a }

-- TODO: What is the right precedence?
infixr 1 -.->

fn   :: (f a -> f' a) -> (f -.-> f') a
fn_2 :: (f a -> f' a -> f'' a) -> (f -.-> f' -.-> f'') a
fn_3 :: (f a -> f' a -> f'' a -> f''' a) -> (f -.-> f' -.-> f'' -.-> f''') a
fn_4 :: (f a -> f' a -> f'' a -> f''' a -> f'''' a) -> (f -.-> f' -.-> f'' -.-> f''' -.-> f'''') a

fn   f = Fn $ \x -> f x
fn_2 f = Fn $ \x -> Fn $ \x' -> f x x'
fn_3 f = Fn $ \x -> Fn $ \x' -> Fn $ \x'' -> f x x' x''
fn_4 f = Fn $ \x -> Fn $ \x' -> Fn $ \x'' -> Fn $ \x''' -> f x x' x'' x'''

-- | Maps a structure containing sums to the corresponding
-- product structure.
type family Prod (h :: (k -> *) -> (l -> *)) :: (k -> *) -> (l -> *)

-- | A generalization of 'Control.Applicative.<*>'.
class (Prod (Prod h) ~ Prod h, HPure (Prod h)) => HAp (h  :: (k -> *) -> (l -> *)) where

  -- | Corresponds to 'Control.Applicative.<*>'.
  --
  -- For products as well as products or products, the correspondence
  -- is rather direct. We combine a structure containing (lifted)
  -- functions and a compatible structure containing corresponding arguments
  -- into a compatible structure containing results.
  --
  -- The same combinator can also be used to combine a product
  -- structure of functions with a sum structure of arguments, which then
  -- results in another sum structure of results. The sum structure
  -- determines which part of the product structure will be used.
  --
  -- /Instances:/
  --
  -- @
  -- 'hap', 'Generics.SOP.NP.ap_NP'  :: 'Generics.SOP.NP.NP'  (f -.-> g) xs -> 'Generics.SOP.NP.NP'  f xs -> 'Generics.SOP.NP.NP'  g xs
  -- 'hap', 'Generics.SOP.NS.ap_NS'  :: 'Generics.SOP.NS.NP'  (f -.-> g) xs -> 'Generics.SOP.NS.NS'  f xs -> 'Generics.SOP.NS.NS'  g xs
  -- 'hap', 'Generics.SOP.NP.ap_POP' :: 'Generics.SOP.NP.POP' (f -.-> g) xs -> 'Generics.SOP.NP.POP' f xs -> 'Generics.SOP.NP.POP' g xs
  -- 'hap', 'Generics.SOP.NS.ap_SOP' :: 'Generics.SOP.NS.POP' (f -.-> g) xs -> 'Generics.SOP.NS.SOP' f xs -> 'Generics.SOP.NS.SOP' g xs
  -- @
  --
  hap :: Prod h (f -.-> g) xs -> h f xs -> h g xs

{-------------------------------------------------------------------------------
  Derived from application
-------------------------------------------------------------------------------}

hliftA  :: (SingI xs, HAp h)               => (forall a. f a -> f' a)                                                   -> h f   xs -> h f'   xs
hliftA2 :: (SingI xs, HAp h, HAp (Prod h)) => (forall a. f a -> f' a -> f'' a)           -> Prod h f xs                 -> h f'  xs -> h f''  xs
hliftA3 :: (SingI xs, HAp h, HAp (Prod h)) => (forall a. f a -> f' a -> f'' a -> f''' a) -> Prod h f xs -> Prod h f' xs -> h f'' xs -> h f''' xs

hliftA  f xs       = hpure (fn   f) `hap` xs
hliftA2 f xs ys    = hpure (fn_2 f) `hap` xs `hap` ys
hliftA3 f xs ys zs = hpure (fn_3 f) `hap` xs `hap` ys `hap` zs

hcliftA  :: (AllMap (Prod h) c xs, SingI xs, HAp h)               => Proxy c -> (forall a. c a => f a -> f' a)                                                   -> h f   xs -> h f'   xs
hcliftA2 :: (AllMap (Prod h) c xs, SingI xs, HAp h, HAp (Prod h)) => Proxy c -> (forall a. c a => f a -> f' a -> f'' a)           -> Prod h f xs                 -> h f'  xs -> h f''  xs
hcliftA3 :: (AllMap (Prod h) c xs, SingI xs, HAp h, HAp (Prod h)) => Proxy c -> (forall a. c a => f a -> f' a -> f'' a -> f''' a) -> Prod h f xs -> Prod h f' xs -> h f'' xs -> h f''' xs

hcliftA  p f xs       = hcpure p (fn   f) `hap` xs
hcliftA2 p f xs ys    = hcpure p (fn_2 f) `hap` xs `hap` ys
hcliftA3 p f xs ys zs = hcpure p (fn_3 f) `hap` xs `hap` ys `hap` zs

-- | Maps products to lists, and sums to identities.
type family CollapseTo (h :: (k -> *) -> (l -> *)) :: * -> *

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
  hcollapse :: SingI xs => h (K a) xs -> CollapseTo h a

class HAp h => HSequence (h :: (k -> *) -> (l -> *)) where
  hsequence' :: (SingI xs, Applicative f) => h (f :.: g) xs -> f (h g xs)

hsequence :: (SingI xs, HSequence h) => Applicative f => h f xs -> f (h I xs)
hsequence = hsequence' . hliftA (Comp . fmap I)

-- Special case for K

hsequenceK ::  (SingI xs, Applicative f, HSequence h) => h (K (f a)) xs -> f (h (K a) xs)
hsequenceK = hsequence' . hliftA (Comp . fmap K . unK)
