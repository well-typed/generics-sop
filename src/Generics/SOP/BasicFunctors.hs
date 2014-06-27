{-# LANGUAGE PolyKinds #-}
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
module Generics.SOP.BasicFunctors
  ( K(..)
  , I(..)
  , (:.:)(..)
  ) where

import Control.Applicative
import Data.Foldable (Foldable)
import Data.Monoid (Monoid, mempty, (<>))
import Data.Traversable (Traversable)

-- | The constant type functor.
--
-- Like 'Data.Functor.Constant.Constant', but kind-polymorphic
-- in its second argument and with a shorter name.
--
newtype K (a :: *) (b :: k) = K { unK :: a }
  deriving (Show, Functor, Foldable, Traversable)

instance Monoid a => Applicative (K a) where
  pure _      = K mempty
  K x <*> K y = K (x <> y)

-- | The identity type functor.
--
-- Like 'Data.Functor.Identity.Identity', but with a shorter name.
--
newtype I (a :: *) = I { unI :: a }
  deriving (Show, Functor, Foldable, Traversable)

instance Applicative I where
  pure = I
  I f <*> I x = I (f x)

instance Monad I where
  return = I
  I x >>= f = f x

-- | Composition of functors.
--
-- Like 'Data.Functor.Compose.Compose', but kind-polymorphic
-- and with a shorter name.
--
newtype (:.:) (f :: l -> *) (g :: k -> l) (p :: k) = Comp { unComp :: f (g p) }
  deriving (Show)

infixr 7 :.:

instance (Functor f, Functor g) => Functor (f :.: g) where
  fmap f (Comp x) = Comp (fmap (fmap f) x)

