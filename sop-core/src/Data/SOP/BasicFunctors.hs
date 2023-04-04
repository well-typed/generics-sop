{-# LANGUAGE PolyKinds, DeriveGeneric, UnliftedNewtypes, StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
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
    K(..)
  , unK
  , I(..)
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
  -- * Convenience helpers
  , BoxedType
  ) where

#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup (Semigroup (..))
#endif
import qualified GHC.Generics as GHC

import Data.Functor.Classes

import Control.DeepSeq (NFData(..))
#if MIN_VERSION_deepseq(1,4,3)
import Control.DeepSeq (NFData1(..), NFData2(..))
#endif

import Data.Coerce (coerce)
import GHC.Exts (TYPE, RuntimeRep(BoxedRep), Levity (Lifted, Unlifted))
import Data.Kind (Type)

-- | convenience type to make working with levity polymorphic types easier
type BoxedType :: Levity -> Type
type BoxedType levity = TYPE ('BoxedRep levity)

-- * Basic functors

-- | The constant type functor.
--
-- Like 'Data.Functor.Constant.Constant', but kind-polymorphic
-- in its second argument and with a shorter name.
--
data family K :: forall levin levout k. BoxedType levin -> k -> BoxedType levout
newtype instance K @'Lifted @'Lifted a b = K a
  deriving (Functor, Foldable, Traversable, GHC.Generic)

-- | @since 0.6.0.0
newtype instance K @'Unlifted @'Unlifted a b = UK {unUK :: a}

-- | @since 0.6.0.0
data instance K @'Unlifted @'Lifted a b = ULK {unULK :: a}

-- | @since 0.2.4.0
instance Eq2 K where
    liftEq2 eq _ (K x) (K y) = eq x y
-- | @since 0.2.4.0
instance Ord2 K where
    liftCompare2 comp _ (K x) (K y) = comp x y
-- | @since 0.2.4.0
instance Read2 K where
    liftReadsPrec2 rp _ _ _ = readsData $
         readsUnaryWith rp "K" K
-- | @since 0.2.4.0
instance Show2 K where
    liftShowsPrec2 sp _ _ _ d (K x) = showsUnaryWith sp "K" d x

-- | @since 0.2.4.0
instance (Eq a) => Eq1 (K a) where
    liftEq = liftEq2 (==)
-- | @since 0.2.4.0
instance (Ord a) => Ord1 (K a) where
    liftCompare = liftCompare2 compare
-- | @since 0.2.4.0
instance (Read a) => Read1 (K a) where
    liftReadsPrec = liftReadsPrec2 readsPrec readList
-- | @since 0.2.4.0
instance (Show a) => Show1 (K a) where
    liftShowsPrec = liftShowsPrec2 showsPrec showList

-- This have to be implemented manually, K is polykinded.
instance (Eq a) => Eq (K a b) where
    K x == K y = x == y
instance (Ord a) => Ord (K a b) where
    compare (K x) (K y) = compare x y
instance (Read a) => Read (K a b) where
    readsPrec = readsData $ readsUnaryWith readsPrec "K" K
instance (Show a) => Show (K a b) where
    showsPrec d (K x) = showsUnaryWith showsPrec "K" d x

-- | @since 0.4.0.0
instance Semigroup a => Semigroup (K a b) where
  K x <> K y = K (x <> y)

-- | @since 0.4.0.0
instance Monoid a => Monoid (K a b) where
  mempty              = K mempty
#if !MIN_VERSION_base(4,11,0)
  mappend (K x) (K y) = K (mappend x y)
#endif

instance Monoid a => Applicative (K a) where
  pure _      = K mempty
  K x <*> K y = K (mappend x y)

-- | @since 0.6.0.0
unK :: K a b -> a
unK (K x) = x

-- | The identity type functor.
--
-- Like 'Data.Functor.Identity.Identity', but with a shorter name.
--
data family I :: forall levin levout. BoxedType levin -> BoxedType levout
newtype instance I @'Lifted @'Lifted a = I a
  deriving (Functor, Foldable, Traversable, GHC.Generic)

-- | @since 0.6.0.0
newtype instance I @'Unlifted @'Unlifted a = UI {unUI :: a}

-- | @since 0.6.0.0
data instance I @'Unlifted @'Lifted a = ULI {unULI :: a}

-- | @since 0.4.0.0
instance Semigroup a => Semigroup (I a) where
  I x <> I y = I (x <> y)

-- | @since 0.4.0.0
instance Monoid a => Monoid (I a) where
  mempty              = I mempty
#if !MIN_VERSION_base(4,11,0)
  mappend (I x) (I y) = I (mappend x y)
#endif

instance Applicative I where
  pure = I
  I f <*> I x = I (f x)

instance Monad I where
  I x >>= f = f x


-- | @since 0.2.4.0
instance Eq1 I where
    liftEq eq (I x) (I y) = eq x y
-- | @since 0.2.4.0
instance Ord1 I where
    liftCompare comp (I x) (I y) = comp x y
-- | @since 0.2.4.0
instance Read1 I where
    liftReadsPrec rp _ = readsData $
         readsUnaryWith rp "I" I
-- | @since 0.2.4.0
instance Show1 I where
    liftShowsPrec sp _ d (I x) = showsUnaryWith sp "I" d x

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
data family (:.:) :: forall levin levout k l.
  (l -> BoxedType levin) -> (k -> l) -> k -> BoxedType levout
newtype instance (:.:) @'Lifted @'Lifted f g p = Comp (f (g p))
  deriving (GHC.Generic)

-- | @since 0.6.0.0
newtype instance (:.:) @'Unlifted @'Unlifted f g p = UComp {unUComp :: f (g p)}

-- | @since 0.6.0.0
data instance (:.:) @'Unlifted @'Lifted f g p = ULComp {unULComp :: f (g p)}

infixr 7 :.:

-- | @since 0.4.0.0
instance (Semigroup (f (g x))) => Semigroup ((f :.: g) x) where
  Comp x <> Comp y = Comp (x <> y)

-- | @since 0.4.0.0
instance (Monoid (f (g x))) => Monoid ((f :.: g) x) where
  mempty                    = Comp mempty
#if !MIN_VERSION_base(4,11,0)
  mappend (Comp x) (Comp y) = Comp (mappend x y)
#endif

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
instance NFData a => NFData (I a) where
    rnf (I x) = rnf x

-- | @since 0.2.5.0
instance NFData a => NFData (K a b) where
    rnf (K x) = rnf x

-- | @since 0.2.5.0
instance NFData (f (g a)) => NFData ((f :.: g)  a) where
    rnf (Comp x) = rnf x

#if MIN_VERSION_deepseq(1,4,3)
-- | @since 0.2.5.0
instance NFData1 I where
    liftRnf r (I x) = r x

-- | @since 0.2.5.0
instance NFData a => NFData1 (K a) where
    liftRnf _ (K x) = rnf x

-- | @since 0.2.5.0
instance NFData2 K where
    liftRnf2 r _ (K x) = r x

-- | @since 0.2.5.0
instance (NFData1 f, NFData1 g) => NFData1 (f :.: g) where
    liftRnf r (Comp x) = liftRnf (liftRnf r) x
#endif

-- | Extract the contents of a 'Comp' value.
unComp :: (f :.: g) p -> f (g p)
unComp (Comp x) = x

-- * Mapping functions

-- | Lift the given function.
--
-- @since 0.2.5.0
--
mapII :: (a -> b) -> I a -> I b
mapII = coerce
{-# INLINE mapII #-}

-- | Lift the given function.
--
-- @since 0.2.5.0
--
mapIK :: (a -> b) -> I a -> K b c
mapIK = coerce
{-# INLINE mapIK #-}

-- | Lift the given function.
--
-- @since 0.2.5.0
--
mapKI :: (a -> b) -> K a c -> I b
mapKI = coerce
{-# INLINE mapKI #-}

-- | Lift the given function.
--
-- @since 0.2.5.0
--
mapKK :: (a -> b) -> K a c -> K b d
mapKK = coerce
{-# INLINE mapKK #-}

-- | Lift the given function.
--
-- @since 0.2.5.0
--
mapIII :: (a -> b -> c) -> I a -> I b -> I c
mapIII = coerce
{-# INLINE mapIII #-}

-- | Lift the given function.
--
-- @since 0.2.5.0
--
mapIIK :: (a -> b -> c) -> I a -> I b -> K c d
mapIIK = coerce
{-# INLINE mapIIK #-}

-- | Lift the given function.
--
-- @since 0.2.5.0
--
mapIKI :: (a -> b -> c) -> I a -> K b d -> I c
mapIKI = coerce
{-# INLINE mapIKI #-}

-- | Lift the given function.
--
-- @since 0.2.5.0
--
mapIKK :: (a -> b -> c) -> I a -> K b d -> K c e
mapIKK = coerce
{-# INLINE mapIKK #-}

-- | Lift the given function.
--
-- @since 0.2.5.0
--
mapKII :: (a -> b -> c) -> K a d -> I b -> I c
mapKII = coerce
{-# INLINE mapKII #-}

-- | Lift the given function.
--
-- @since 0.2.5.0
--
mapKIK :: (a -> b -> c) -> K a d -> I b -> K c e
mapKIK = coerce
{-# INLINE mapKIK #-}

-- | Lift the given function.
--
-- @since 0.2.5.0
--
mapKKI :: (a -> b -> c) -> K a d -> K b e -> I c
mapKKI = coerce
{-# INLINE mapKKI #-}

-- | Lift the given function.
--
-- @since 0.2.5.0
--
mapKKK :: (a -> b -> c) -> K a d -> K b e -> K c f
mapKKK = coerce
{-# INLINE mapKKK #-}
