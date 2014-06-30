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

-- | n-ary sum
data NS :: (k -> *) -> [k] -> * where
  Z :: f x -> NS f (x ': xs)
  S :: NS f xs -> NS f (x ': xs)

deriving instance All Show (Map f xs) => Show (NS f xs)
deriving instance All Eq   (Map f xs) => Eq   (NS f xs)
deriving instance (All Eq (Map f xs), All Ord (Map f xs)) => Ord (NS f xs)

-- | n-ary sum-of-prodcuts
newtype SOP (f :: (k -> *)) (xss :: [[k]]) = SOP { unSOP :: NS (NP f) xss }
  deriving (Show, Eq, Ord)

{-------------------------------------------------------------------------------
  Constructing sums
-------------------------------------------------------------------------------}

type Injection (f :: k -> *) (xs :: [k]) = f -.-> K (NS f xs)

injections :: forall xs f. SingI xs => NP (Injection f xs) xs
injections = case sing :: Sing xs of
  SNil   -> Nil
  SCons  -> fn (K . Z) :* liftA_NP shift injections

shift :: Injection f xs a -> Injection f (x ': xs) a
shift (Fn f) = Fn $ K . S . unK . f

apInjs_NP  :: SingI xs  => NP  f xs  -> [NS  f xs]
apInjs_NP  = hcollapse . hap injections

apInjs_POP :: SingI xss => POP f xss -> [SOP f xss]
apInjs_POP = map SOP . apInjs_NP . unPOP

ap_NS :: NP (f -.-> g) xs -> NS f xs -> NS g xs
ap_NS (Fn f  :* _)   (Z x)   = Z (f x)
ap_NS (_     :* fs)  (S xs)  = S (ap_NS fs xs)
ap_NS _ _ = error "inaccessible"

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

collapse_NS  ::              NS  (K a) xs  ->   a
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

