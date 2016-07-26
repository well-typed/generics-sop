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

-- | The constant type functor.
--
-- Like 'Data.Functor.Constant.Constant', but kind-polymorphic
-- in its second argument and with a shorter name.
--
newtype K (a :: *) (b :: k) = K a
#if MIN_VERSION_base(4,7,0)
  deriving (Show, Functor, Foldable, Traversable, GHC.Generic)
#else
  deriving (Show, GHC.Generic)

instance Functor (K a) where
  fmap _ (K x) = K x

instance Foldable (K a) where
  foldr _ z (K _) = z
  foldMap _ (K _) = mempty

instance Traversable (K a) where
  traverse _ (K x) = pure (K x)
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
  deriving (Show, Functor, Foldable, Traversable, GHC.Generic)
#else
  deriving (Show, GHC.Generic)

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

-- | Extract the contents of an 'I' value.
unI :: I a -> a
unI (I x) = x

-- | Composition of functors.
--
-- Like 'Data.Functor.Compose.Compose', but kind-polymorphic
-- and with a shorter name.
--
newtype (:.:) (f :: l -> *) (g :: k -> l) (p :: k) = Comp (f (g p))
  deriving (Show, GHC.Generic)

infixr 7 :.:

instance (Functor f, Functor g) => Functor (f :.: g) where
  fmap f (Comp x) = Comp (fmap (fmap f) x)

-- | Extract the contents of a 'Comp' value.
unComp :: (f :.: g) p -> f (g p)
unComp (Comp x) = x

