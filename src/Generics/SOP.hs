{-# LANGUAGE PolyKinds, UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
-- | Main module of @generics-sop@
--
-- In most cases, you will probably want to import just this module,
-- and possibly "Generics.SOP.TH" if you want to use Template Haskell
-- to generate 'Generic' instances for you.
--
-- = Generic programming with sums of products
--
-- You need this library if you want to define your own generic functions
-- in the sum-of-products SOP style. Generic programming in the SOP style
-- follows the following idea:
--
--   1.  A large class of datatypes can be viewed in a uniform, structured
--       way: the choice between constructors is represented using an n-ary
--       sum (called 'NS'), and the arguments of each constructor are
--       represented using an n-ary product (called 'NP').
--
--   2.  The library captures the notion of a datatype being representable
--       in the following way. There is a class 'Generic', which for a given
--       datatype @A@, associates the isomorphic SOP representation with
--       the original type under the name @'Rep' A@. The class also provides
--       functions 'from' and 'to' that convert between @A@ and @'Rep' A@ and
--       witness the isomorphism.
--
--   3.  Since all 'Rep' types are sums of products, you can define
--       functions over them by performing induction on the structure, of
--       by using predefined combinators that the library provides. Such
--       functions then work for all 'Rep' types.
--
--   4.  By combining the conversion functions 'from' and 'to' with the
--       function that works on 'Rep' types, we obtain a function that works
--       on all types that are in the 'Generic' class.
--
--   5.  Most types can very easily be made an instance of 'Generic'. For
--       example, if the datatype can be represented using GHC's built-in
--       approach to generic programming and has an instance for the
--       'GHC.Generics.Generic' class from module "GHC.Generics", then an
--       instance of the SOP 'Generic' can automatically be derived. There
--       is also Template Haskell code in "Generics.SOP.TH" that allows to
--       auto-generate an instance of 'Generic' for most types.
--
-- = Example
--
-- == Instantiating a datatype for use with SOP generics
--
-- Let's assume we have the datatype:
--
-- > data A = C Bool | D A Int | E
--
-- To create a 'Generic' instance for @A@ via "GHC.Generics", we say
--
-- > {-# LANGUAGE DeriveGenerics #-}
-- >
-- > import qualified GHC.Generics as GHC
-- > import Generics.SOP
-- >
-- > data A = C Bool | D A Int | E
-- >   deriving (Show, GHC.Generic)
-- >
-- > instance Generic A -- empty
--
-- Now we can convert between @A@ and @'Rep' A@. For example,
--
-- >>> from (D (C True) 3) :: Rep A
-- > SOP (S (Z (I (C True) :* I 3 :* Nil)))
-- >>> to it :: A
-- > D (C True) 3
--
-- == Defining a generic function
--
-- Here's a definition of generic equality in SOP style, making use of
-- structural recursion on the sums and products:
--
-- > geq :: (Generic a, All2 Eq (Code a)) => a -> a -> Bool
-- > geq x y = goS (from x) (from y)
-- >   where
-- >     goS :: (All2 Eq xss) => SOP I xss -> SOP I xss -> Bool
-- >     goS (SOP (Z xs))  (SOP (Z ys))  = goP xs ys
-- >     goS (SOP (S xss)) (SOP (S yss)) = goS (SOP xss) (SOP yss)
-- >     goS _             _             = False
-- >
-- >     goP :: (All Eq xs) => NP I xs -> NP I xs -> Bool
-- >     goP Nil         Nil         = True
-- >     goP (I x :* xs) (I y :* ys) = x == y && goP xs ys
--
-- The 'All2' and 'All' constraints say that all components that occur
-- in the definition of type @a@ must be in the 'Eq' class.
--
-- The local functions @goS@ and @goP@ operate on the structural
-- representations (note that @'Rep' A = 'SOP' 'I' ('Code' A)@).
--
-- The wrapper just invokes the conversion function @from@ on the
-- arguments and then dispatches to the helper functions.
--
-- In many cases, generic functions can be written in a much more
-- concise way by avoiding the explicit structural recursion and
-- resorting to the powerful combinators provided by this library
-- instead.
--
-- == Using a generic function
--
-- We can directly invoke 'geq' on any type that is an instance of
-- class 'Generic'. However, the type of 'geq' requires that all components
-- of a type it is invoked on are in 'Eq'. Since for example @A@ is
-- recursive, this means that we have to make @A@ an instance of 'Eq'
-- in order to use it. But we can use 'geq' as the definition:
--
-- > instance Eq A where
-- >   (==) = geq
--
-- Now we can use the function:
--
-- >>> D (C True) 3 == D (C True) 3
-- True
-- >>> D (C True) 3 == D (C True) 4
-- False
--
-- = More examples
--
-- The best way to learn about how to define generic functions in the SOP style
-- is to look at a few simple examples. Examples are provided by the following
-- packages:
--
--   * @<http://hackage.haskell.org/packages/basic-sop basic-sop>@ basic examples,
--   * @<http://hackage.haskell.org/packages/pretty-sop pretty-sop>@ generic pretty printing,
--   * @<http://hackage.haskell.org/packages/lens-sop lens-sop>@ generically computed lenses,
--   * @<http://hackage.haskell.org/packages/json-sop json-sop>@ generic JSON conversions.
--
-- The generic functions in these package use a wide variety of the combinators
-- that are offered by the library.
--
-- = Paper
--
-- A detailed description of the ideas behind this library is provided by
-- the paper:
--
--   * Edsko de Vries and Andres Löh.
--     <http://www.andres-loeh.de/TrueSumsOfProducts True Sums of Products>.
--     Workshop on Generic Programming (WGP) 2014.
--
--
module Generics.SOP (
    -- * Codes and interpretations
    Generic(..)
  , Rep
    -- * n-ary datatypes
  , NP(..)
  , NS(..)
  , SOP(..)
  , POP(..)
    -- * Metadata
  , DatatypeName
  , ModuleName
  , ConstructorName
  , FieldName
  , DatatypeInfo(..)
  , ConstructorInfo(..)
  , FieldInfo(..)
  , HasDatatypeInfo(..)
    -- * Combinators
    -- ** Constructing products
  , HPure(..)
    -- ** Application
  , (-.->)(..)
  , fn
  , fn_2
  , fn_3
  , fn_4
  , Prod
  , HAp(..)
    -- ** Derived application
  , hliftA
  , hliftA2
  , hliftA3
  , hcliftA
  , hcliftA2
  , hcliftA3
    -- ** Constructing sums
  , Injection
  , injections
  , shift
  , apInjs_NP
  , apInjs_POP
    -- ** Dealing with 'All c'
  , AllDict(..)
  , allDict_NP
  , hcliftA'
  , hcliftA2'
  , hcliftA3'
    -- ** Collapsing
  , CollapseTo
  , HCollapse(..)
    -- ** Sequencing
  , HSequence(..)
  , hsequence
  , hsequenceK
    -- ** Partial operations
  , fromList
    -- * Utilities
    -- ** Basic functors
  , K(..)
  , I(..)
  , (:.:)(..)
    -- ** Mapping constraints
  , All
  , All2
  , Map
  , AllMap
    -- ** Singletons
  , Sing(..)
  , SingI(..)
    -- * Shape of type-level lists
  , Shape(..)
  , shape
  , lengthSing
    -- * Re-exports
  , Proxy(..)
  ) where

import Data.Proxy (Proxy(..))

import Generics.SOP.BasicFunctors
import Generics.SOP.Classes
import Generics.SOP.Constraint
import Generics.SOP.Instances ()
import Generics.SOP.Metadata
import Generics.SOP.NP
import Generics.SOP.NS
import Generics.SOP.Universe
import Generics.SOP.Sing

