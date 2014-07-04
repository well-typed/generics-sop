{-# LANGUAGE PolyKinds, StandaloneDeriving, UndecidableInstances #-}
#if MIN_VERSION_base(4,7,0)
{-# LANGUAGE AutoDeriveTypeable #-}
#endif
-- n-ary sums (and sums of products)
module Generics.SOP.NS where

import Control.Applicative
import Data.Proxy (Proxy(..))

import Generics.SOP.BasicFunctors
import Generics.SOP.Classes
import Generics.SOP.Constraint
import Generics.SOP.NP
import Generics.SOP.Sing

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
-- > S (Z (I 1))    :: NS (K Int) '[ Char, Bool ]
--
data NS :: (k -> *) -> [k] -> * where
  Z :: f x -> NS f (x ': xs)
  S :: NS f xs -> NS f (x ': xs)

deriving instance All Show (Map f xs) => Show (NS f xs)
deriving instance All Eq   (Map f xs) => Eq   (NS f xs)
deriving instance (All Eq (Map f xs), All Ord (Map f xs)) => Ord (NS f xs)

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
newtype SOP (f :: (k -> *)) (xss :: [[k]]) = SOP { unSOP :: NS (NP f) xss }
  deriving (Show, Eq, Ord)

{-------------------------------------------------------------------------------
  Constructing sums
-------------------------------------------------------------------------------}

-- | The type of injections into an n-ary sum.
--
-- If you expand the type synonyms and newtypes involved, you get
--
-- > Injection f xs a = (f -.-> K (NS f xs)) a ~= f a -> K (NS f xs) a ~= f a -> K (NS f xs)
--
-- If we pick @a@ to be an element of @xs@, this indeed corresponds to an
-- injection into the sum.
--
type Injection (f :: k -> *) (xs :: [k]) = f -.-> K (NS f xs)

-- | Compute all injections into an n-ary sum.
--
-- Each element of the resulting product contains one of the injections.
--
injections :: forall xs f. SingI xs => NP (Injection f xs) xs
injections = case sing :: Sing xs of
  SNil   -> Nil
  SCons  -> fn (K . Z) :* liftA_NP shift injections

-- | Shift an injection.
--
-- Given an injection, return an injection into a sum that is one component larger.
--
shift :: Injection f xs a -> Injection f (x ': xs) a
shift (Fn f) = Fn $ K . S . unK . f

apInjs_NP  :: SingI xs  => NP  f xs  -> [NS  f xs]
apInjs_NP  = hcollapse . hap injections

apInjs_POP :: SingI xss => POP f xss -> [SOP f xss]
apInjs_POP = map SOP . apInjs_NP . unPOP

-- | Specialization of 'hap'.
ap_NS :: NP (f -.-> g) xs -> NS f xs -> NS g xs
ap_NS (Fn f  :* _)   (Z x)   = Z (f x)
ap_NS (_     :* fs)  (S xs)  = S (ap_NS fs xs)
ap_NS _ _ = error "inaccessible"

-- | Specialization of 'hap'.
ap_SOP  :: POP (f -.-> g) xs -> SOP  f xs -> SOP  g xs
ap_SOP (POP (fs :* _)  ) (SOP (Z xs) ) = SOP (Z (ap_NP  fs  xs))
ap_SOP (POP (_  :* fss)) (SOP (S xss)) = SOP (S (unSOP (ap_SOP (POP fss) (SOP xss))))
ap_SOP _ _ = error "inaccessible"

-- Generalization

type instance Prod NS  = NP
type instance Prod SOP = POP

instance HAp NS  where hap = ap_NS
instance HAp SOP where hap = ap_SOP

-- Specialized functions, mostly to keep in line with the paper:

liftA_NS  :: SingI xs  => (forall a. f a -> g a) -> NS  f xs  -> NS  g xs
liftA_SOP :: SingI xss => (forall a. f a -> g a) -> SOP f xss -> SOP g xss

liftA_NS  = hliftA
liftA_SOP = hliftA

liftA2_NS  :: SingI xs  => (forall a. f a -> g a -> h a) -> NP  f xs  -> NS  g xs  -> NS   h xs
liftA2_SOP :: SingI xss => (forall a. f a -> g a -> h a) -> POP f xss -> SOP g xss -> SOP  h xss

liftA2_NS  = hliftA2
liftA2_SOP = hliftA2

cliftA_NS  :: (All  c xs,  SingI xs)  => Proxy c -> (forall a. c a => f a -> g a) -> NS   f xs  -> NS  g xs
cliftA_SOP :: (All2 c xss, SingI xss) => Proxy c -> (forall a. c a => f a -> g a) -> SOP  f xss -> SOP g xss

cliftA_NS  = hcliftA
cliftA_SOP = hcliftA

cliftA2_NS  :: (All  c xs,  SingI xs)  => Proxy c -> (forall a. c a => f a -> g a -> h a) -> NP  f xs  -> NS  g xs  -> NS  h xs
cliftA2_SOP :: (All2 c xss, SingI xss) => Proxy c -> (forall a. c a => f a -> g a -> h a) -> POP f xss -> SOP g xss -> SOP h xss

cliftA2_NS  = hcliftA2
cliftA2_SOP = hcliftA2

{-------------------------------------------------------------------------------
  Dealing with 'All c'
-------------------------------------------------------------------------------}

-- Specializations, to keep in line with the paper

cliftA2'_NS :: (All2 c xss, SingI xss) => Proxy c -> (forall xs. (SingI xs, All c xs) => f xs -> g xs -> h xs) -> NP f xss -> NS g xss -> NS h xss

cliftA2'_NS = hcliftA2'

{-------------------------------------------------------------------------------
  Collapsing
-------------------------------------------------------------------------------}

-- | Specialization of 'hcollapse'.
collapse_NS  ::              NS  (K a) xs  ->   a
-- | Specialization of 'hcollapse'.
collapse_SOP :: SingI xss => SOP (K a) xss ->  [a]

collapse_NS (Z (K x)) = x
collapse_NS (S xs)    = collapse_NS xs

collapse_SOP = collapse_NS . hliftA (K . collapse_NP) . unSOP

-- Generalization

type instance CollapseTo NS  = I
type instance CollapseTo SOP = []

instance HCollapse NS  where hcollapse = I . collapse_NS
instance HCollapse SOP where hcollapse = collapse_SOP

{-------------------------------------------------------------------------------
  Sequencing
-------------------------------------------------------------------------------}

sequence'_NS  ::             Applicative f  => NS  (f :.: g) xs  -> f (NS  g xs)
sequence'_SOP :: (SingI xss, Applicative f) => SOP (f :.: g) xss -> f (SOP g xss)

sequence'_NS (Z mx)  = Z <$> unComp mx
sequence'_NS (S mxs) = S <$> sequence'_NS mxs

sequence'_SOP = fmap SOP . sequence'_NS . hliftA (Comp . sequence'_NP) . unSOP

-- Generalization

instance HSequence NS  where hsequence' = sequence'_NS
instance HSequence SOP where hsequence' = sequence'_SOP

-- Specializations, to keep in line with the paper

sequence_NS  :: (SingI xs,  Applicative f) => NS  f xs  -> f (NS  I xs)
sequence_SOP :: (SingI xss, Applicative f) => SOP f xss -> f (SOP I xss)

sequence_NS   = hsequence
sequence_SOP  = hsequence

