{-# LANGUAGE PolyKinds, DeriveGeneric, PatternSynonyms #-}
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
module Data.SOP.BasicFunctors
  ( -- * Basic functors
    K
  , Const(.., K)
  , unK
  , I
  , Identity(.., I)
  , unI
  , (:.:)(..)
  , unComp
    -- * Mapping functions
  , mapII
  , mapIK
  , mapKI
  , mapKK
  , mapIII
  , mapIIK
  , mapIKI
  , mapIKK
  , mapKII
  , mapKIK
  , mapKKI
  , mapKKK
  ) where

import Data.Semigroup (Semigroup (..))
import Data.Kind (Type)
import qualified GHC.Generics as GHC

import Data.Functor.Classes
import Data.Functor.Const (Const(..))
import Data.Functor.Identity (Identity(..))

import Control.DeepSeq (NFData(..))
#if MIN_VERSION_deepseq(1,4,3)
import Control.DeepSeq (NFData1(..))
#endif

-- * Basic functors

-- | Type and pattern synonyms for the constant type functor.
type K = Const

{-# COMPLETE K #-}
pattern K :: a -> Const a b
pattern K a = Const a

-- | Extract the contents of a 'K' value.
unK :: K a b -> a
unK (K x) = x

-- | Type and pattern synonyms for the identity type functor.
type I = Identity

{-# COMPLETE I #-}
pattern I :: a -> Identity a
pattern I a = Identity a

-- | Extract the contents of an 'I' value.
unI :: I a -> a
unI (I x) = x

-- | Composition of functors.
--
-- Like 'Data.Functor.Compose.Compose', but kind-polymorphic
-- and with a shorter name.
--
newtype (:.:) (f :: l -> Type) (g :: k -> l) (p :: k) = Comp (f (g p))
  deriving (GHC.Generic)

infixr 7 :.:

-- | @since 0.4.0.0
instance (Semigroup (f (g x))) => Semigroup ((f :.: g) x) where
  Comp x <> Comp y = Comp (x <> y)

-- | @since 0.4.0.0
instance (Monoid (f (g x))) => Monoid ((f :.: g) x) where
  mempty                    = Comp mempty
  mappend (Comp x) (Comp y) = Comp (mappend x y)

instance (Functor f, Functor g) => Functor (f :.: g) where
  fmap f (Comp x) = Comp (fmap (fmap f) x)

-- | @since 0.2.5.0
instance (Applicative f, Applicative g) => Applicative (f :.: g) where
  pure x = Comp (pure (pure x))
  Comp f <*> Comp x = Comp ((<*>) <$> f <*> x)

-- | @since 0.2.5.0
instance (Foldable f, Foldable g) => Foldable (f :.: g) where
  foldMap f (Comp t) = foldMap (foldMap f) t

-- | @since 0.2.5.0
instance (Traversable f, Traversable g) => Traversable (f :.: g) where
  traverse f (Comp t) = Comp <$> traverse (traverse f) t


-- Instances of lifted Prelude classes

-- | @since 0.2.4.0
instance (Eq1 f, Eq1 g) => Eq1 (f :.: g) where
    liftEq eq (Comp x) (Comp y) = liftEq (liftEq eq) x y

-- | @since 0.2.4.0
instance (Ord1 f, Ord1 g) => Ord1 (f :.: g) where
    liftCompare comp (Comp x) (Comp y) =
        liftCompare (liftCompare comp) x y

-- | @since 0.2.4.0
instance (Read1 f, Read1 g) => Read1 (f :.: g) where
    liftReadsPrec rp rl = readsData $
        readsUnaryWith (liftReadsPrec rp' rl') "Comp" Comp
      where
        rp' = liftReadsPrec rp rl
        rl' = liftReadList rp rl

-- | @since 0.2.4.0
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

-- NFData Instances

-- | @since 0.2.5.0
instance NFData (f (g a)) => NFData ((f :.: g)  a) where
    rnf (Comp x) = rnf x

#if MIN_VERSION_deepseq(1,4,3)

-- | @since 0.2.5.0
instance (NFData1 f, NFData1 g) => NFData1 (f :.: g) where
    liftRnf r (Comp x) = liftRnf (liftRnf r) x
#endif

-- | Extract the contents of a 'Comp' value.
unComp :: (f :.: g) p -> f (g p)
unComp (Comp x) = x

-- * Mapping functions

-- Implementation note:
--
-- All of these functions are just type specializations of
-- 'coerce'. However, we currently still support GHC 7.6
-- which does not support 'coerce', so we write them
-- explicitly.

-- | Lift the given function.
--
-- @since 0.2.5.0
--
mapII :: (a -> b) -> I a -> I b
mapII = \ f (I a) -> I (f a)
{-# INLINE mapII #-}

-- | Lift the given function.
--
-- @since 0.2.5.0
--
mapIK :: (a -> b) -> I a -> K b c
mapIK = \ f (I a) -> K (f a)
{-# INLINE mapIK #-}

-- | Lift the given function.
--
-- @since 0.2.5.0
--
mapKI :: (a -> b) -> K a c -> I b
mapKI = \ f (K a) -> I (f a)
{-# INLINE mapKI #-}

-- | Lift the given function.
--
-- @since 0.2.5.0
--
mapKK :: (a -> b) -> K a c -> K b d
mapKK = \ f (K a) -> K (f a)
{-# INLINE mapKK #-}

-- | Lift the given function.
--
-- @since 0.2.5.0
--
mapIII :: (a -> b -> c) -> I a -> I b -> I c
mapIII = \ f (I a) (I b) -> I (f a b)
{-# INLINE mapIII #-}

-- | Lift the given function.
--
-- @since 0.2.5.0
--
mapIIK :: (a -> b -> c) -> I a -> I b -> K c d
mapIIK = \ f (I a) (I b) -> K (f a b)
{-# INLINE mapIIK #-}

-- | Lift the given function.
--
-- @since 0.2.5.0
--
mapIKI :: (a -> b -> c) -> I a -> K b d -> I c
mapIKI = \ f (I a) (K b) -> I (f a b)
{-# INLINE mapIKI #-}

-- | Lift the given function.
--
-- @since 0.2.5.0
--
mapIKK :: (a -> b -> c) -> I a -> K b d -> K c e
mapIKK = \ f (I a) (K b) -> K (f a b)
{-# INLINE mapIKK #-}

-- | Lift the given function.
--
-- @since 0.2.5.0
--
mapKII :: (a -> b -> c) -> K a d -> I b -> I c
mapKII = \ f (K a) (I b) -> I (f a b)
{-# INLINE mapKII #-}

-- | Lift the given function.
--
-- @since 0.2.5.0
--
mapKIK :: (a -> b -> c) -> K a d -> I b -> K c e
mapKIK = \ f (K a) (I b) -> K (f a b)
{-# INLINE mapKIK #-}

-- | Lift the given function.
--
-- @since 0.2.5.0
--
mapKKI :: (a -> b -> c) -> K a d -> K b e -> I c
mapKKI = \ f (K a) (K b) -> I (f a b)
{-# INLINE mapKKI #-}

-- | Lift the given function.
--
-- @since 0.2.5.0
--
mapKKK :: (a -> b -> c) -> K a d -> K b e -> K c f
mapKKK = \ f (K a) (K b) -> K (f a b)
{-# INLINE mapKKK #-}
