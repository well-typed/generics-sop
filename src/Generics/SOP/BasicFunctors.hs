{-# LANGUAGE PolyKinds, DeriveGeneric #-}
-- | Basic functors.
--
-- Definitions of the type-level equivalents of
-- 'const', 'id', and ('.'), and a definition of
-- the lifted function space.
--
-- These datatypes are generally useful, but in this
-- library, they're primarily used as parameters for
-- the 'NP', 'NS', 'POP', and 'SOP' types.
--
-- We define own variants of 'Control.Applicative.Const',
-- 'Data.Functor.Identity.Identity' and 'Data.Functor.Compose.Compose' for
-- various reasons.
--
-- * 'Control.Applicative.Const' and 'Data.Functor.Compose.Compose' become
-- kind polymorphic only in @base-4.9.0.0@ (@transformers-0.5.0.0@).
--
-- * Shorter names are convenient, and pattern synonyms aren't
-- (yet) powerful enough, particularly exhaustiveness check doesn't work
-- properly. See <https://ghc.haskell.org/trac/ghc/ticket/8779>.
--
module Generics.SOP.BasicFunctors
  ( K(..)
  , unK
  , I(..)
  , unI
  , (:.:)(..)
  , unComp
  ) where

#if MIN_VERSION_base(4,8,0)
import Data.Monoid ((<>))
#else
import Control.Applicative
import Data.Foldable (Foldable(..))
import Data.Monoid (Monoid, mempty, (<>))
import Data.Traversable (Traversable(..))
#endif
import qualified GHC.Generics as GHC

import Data.Functor.Classes

#if MIN_VERSION_base(4,9,0)
#define LIFTED_CLASSES 1
#else
#if MIN_VERSION_transformers(0,5,0)
#define LIFTED_CLASSES 1
#else
#if MIN_VERSION_transformers_compat(0,5,0) && !MIN_VERSION_transformers(0,4,0)
#define LIFTED_CLASSES 1
#endif
#endif
#endif

-- | The constant type functor.
--
-- Like 'Data.Functor.Constant.Constant', but kind-polymorphic
-- in its second argument and with a shorter name.
--
newtype K (a :: *) (b :: k) = K a
#if MIN_VERSION_base(4,7,0)
  deriving (Functor, Foldable, Traversable, GHC.Generic)
#else
  deriving (GHC.Generic)

instance Functor (K a) where
  fmap _ (K x) = K x

instance Foldable (K a) where
  foldr _ z (K _) = z
  foldMap _ (K _) = mempty

instance Traversable (K a) where
  traverse _ (K x) = pure (K x)
#endif

#ifdef LIFTED_CLASSES
instance Eq2 K where
    liftEq2 eq _ (K x) (K y) = eq x y
instance Ord2 K where
    liftCompare2 comp _ (K x) (K y) = comp x y
instance Read2 K where
    liftReadsPrec2 rp _ _ _ = readsData $
         readsUnaryWith rp "K" K
instance Show2 K where
    liftShowsPrec2 sp _ _ _ d (K x) = showsUnaryWith sp "K" d x

instance (Eq a) => Eq1 (K a) where
    liftEq = liftEq2 (==)
instance (Ord a) => Ord1 (K a) where
    liftCompare = liftCompare2 compare
instance (Read a) => Read1 (K a) where
    liftReadsPrec = liftReadsPrec2 readsPrec readList
instance (Show a) => Show1 (K a) where
    liftShowsPrec = liftShowsPrec2 showsPrec showList
#else
instance (Eq a) => Eq1 (K a) where
    eq1 (K x) (K y) = x == y
instance (Ord a) => Ord1 (K a) where
    compare1 (K x) (K y) = compare x y
instance (Read a) => Read1 (K a) where
    readsPrec1 = readsData $ readsUnary "K" K
instance (Show a) => Show1 (K a) where
    showsPrec1 d (K x) = showsUnary "K" d x
#endif

-- This have to be implemented manually, K is polykinded.
instance (Eq a) => Eq (K a b) where
    K x == K y = x == y
instance (Ord a) => Ord (K a b) where
    compare (K x) (K y) = compare x y
#ifdef LIFTED_CLASSES
instance (Read a) => Read (K a b) where
    readsPrec = readsData $ readsUnaryWith readsPrec "K" K
instance (Show a) => Show (K a b) where
    showsPrec d (K x) = showsUnaryWith showsPrec "K" d x
#else
instance (Read a) => Read (K a b) where
    readsPrec = readsData $ readsUnary "K" K
instance (Show a) => Show (K a b) where
    showsPrec d (K x) = showsUnary "K" d x
#endif

instance Monoid a => Applicative (K a) where
  pure _      = K mempty
  K x <*> K y = K (x <> y)

-- | Extract the contents of a 'K' value.
unK :: K a b -> a
unK (K x) = x

-- | The identity type functor.
--
-- Like 'Data.Functor.Identity.Identity', but with a shorter name.
--
newtype I (a :: *) = I a
#if MIN_VERSION_base(4,7,0)
  deriving (Functor, Foldable, Traversable, GHC.Generic)
#else
  deriving (GHC.Generic)

instance Functor I where
  fmap f (I x) = I (f x)

instance Foldable I where
  foldr f z (I x) = f x z
  foldMap f (I x) = f x

instance Traversable I where
  traverse f (I x) = fmap I (f x)
#endif

instance Applicative I where
  pure = I
  I f <*> I x = I (f x)

instance Monad I where
  return = I
  I x >>= f = f x


#ifdef LIFTED_CLASSES
instance Eq1 I where
    liftEq eq (I x) (I y) = eq x y
instance Ord1 I where
    liftCompare comp (I x) (I y) = comp x y
instance Read1 I where
    liftReadsPrec rp _ = readsData $
         readsUnaryWith rp "I" I
instance Show1 I where
    liftShowsPrec sp _ d (I x) = showsUnaryWith sp "I" d x
#else
instance Eq1 I where
    eq1 (I x) (I y) = x == y
instance Ord1 I where
    compare1 (I x) (I y) = compare x y
instance Read1 I where
    readsPrec1 = readsData $ readsUnary "I" I
instance Show1 I where
    showsPrec1 d (I x) = showsUnary "I" d x
#endif

instance (Eq a) => Eq (I a) where (==) = eq1
instance (Ord a) => Ord (I a) where compare = compare1
instance (Read a) => Read (I a) where readsPrec = readsPrec1
instance (Show a) => Show (I a) where showsPrec = showsPrec1

-- | Extract the contents of an 'I' value.
unI :: I a -> a
unI (I x) = x

-- | Composition of functors.
--
-- Like 'Data.Functor.Compose.Compose', but kind-polymorphic
-- and with a shorter name.
--
newtype (:.:) (f :: l -> *) (g :: k -> l) (p :: k) = Comp (f (g p))
  deriving (GHC.Generic)

infixr 7 :.:

instance (Functor f, Functor g) => Functor (f :.: g) where
  fmap f (Comp x) = Comp (fmap (fmap f) x)

-- Instances of lifted Prelude classes

#ifdef LIFTED_CLASSES
instance (Eq1 f, Eq1 g) => Eq1 (f :.: g) where
    liftEq eq (Comp x) (Comp y) = liftEq (liftEq eq) x y

instance (Ord1 f, Ord1 g) => Ord1 (f :.: g) where
    liftCompare comp (Comp x) (Comp y) =
        liftCompare (liftCompare comp) x y

instance (Read1 f, Read1 g) => Read1 (f :.: g) where
    liftReadsPrec rp rl = readsData $
        readsUnaryWith (liftReadsPrec rp' rl') "Comp" Comp
      where
        rp' = liftReadsPrec rp rl
        rl' = liftReadList rp rl

instance (Show1 f, Show1 g) => Show1 (f :.: g) where
    liftShowsPrec sp sl d (Comp x) =
        showsUnaryWith (liftShowsPrec sp' sl') "Comp" d x
      where
        sp' = liftShowsPrec sp sl
        sl' = liftShowList sp sl

instance (Eq1 f, Eq1 g, Eq a) => Eq ((f :.: g) a) where (==) = eq1
instance (Ord1 f, Ord1 g, Ord a) => Ord ((f :.: g) a) where compare = compare1
instance (Read1 f, Read1 g, Read a) => Read ((f :.: g) a) where readsPrec = readsPrec1
instance (Show1 f, Show1 g, Show a) => Show ((f :.: g) a) where showsPrec = showsPrec1
#else
-- kludge to get type with the same instances as g a
newtype Apply g a = Apply (g a)

getApply :: Apply g a -> g a
getApply (Apply x) = x

instance (Eq1 g, Eq a) => Eq (Apply g a) where
    Apply x == Apply y = eq1 x y

instance (Ord1 g, Ord a) => Ord (Apply g a) where
    compare (Apply x) (Apply y) = compare1 x y

instance (Read1 g, Read a) => Read (Apply g a) where
    readsPrec d s = [(Apply a, t) | (a, t) <- readsPrec1 d s]

instance (Show1 g, Show a) => Show (Apply g a) where
    showsPrec d (Apply x) = showsPrec1 d x

instance (Functor f, Eq1 f, Eq1 g, Eq a) => Eq ((f :.: g) a) where
    Comp x == Comp y = eq1 (fmap Apply x) (fmap Apply y)

instance (Functor f, Ord1 f, Ord1 g, Ord a) => Ord ((f :.: g) a) where
    compare (Comp x) (Comp y) = compare1 (fmap Apply x) (fmap Apply y)

instance (Functor f, Read1 f, Read1 g, Read a) => Read ((f :.: g) a) where
    readsPrec = readsData $ readsUnary1 "Comp" (Comp . fmap getApply)

instance (Functor f, Show1 f, Show1 g, Show a) => Show ((f :.: g) a) where
    showsPrec d (Comp x) = showsUnary1 "Comp" d (fmap Apply x)

instance (Functor f, Eq1 f, Eq1 g) => Eq1 (f :.: g) where eq1 = (==)
instance (Functor f, Ord1 f, Ord1 g) => Ord1 (f :.: g) where
    compare1 = compare
instance (Functor f, Read1 f, Read1 g) => Read1 (f :.: g) where
    readsPrec1 = readsPrec
instance (Functor f, Show1 f, Show1 g) => Show1 (f :.: g) where
    showsPrec1 = showsPrec
#endif

-- | Extract the contents of a 'Comp' value.
unComp :: (f :.: g) p -> f (g p)
unComp (Comp x) = x
