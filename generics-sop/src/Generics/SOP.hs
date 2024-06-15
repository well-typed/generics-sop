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
--       functions over them by performing induction on the structure, or
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
-- Let's assume we have the datatypes:
--
-- > data A   = C Bool | D A Int | E (B ())
-- > data B a = F | G a Char Bool
--
-- To create 'Generic' instances for @A@ and @B@ via "GHC.Generics", we say
--
-- > {-# LANGUAGE DeriveGeneric #-}
-- >
-- > import qualified GHC.Generics as GHC
-- > import Generics.SOP
-- >
-- > data A   = C Bool | D A Int | E (B ())
-- >   deriving (Show, GHC.Generic)
-- > data B a = F | G a Char Bool
-- >   deriving (Show, GHC.Generic)
-- >
-- > instance Generic A     -- empty
-- > instance Generic (B a) -- empty
--
-- Now we can convert between @A@ and @'Rep' A@ (and between @B@ and @'Rep' B@).
-- For example,
--
-- >>> from (D (C True) 3) :: Rep A
-- SOP (S (Z (I (C True) :* I 3 :* Nil)))
-- >>> to it :: A
-- D (C True) 3
--
-- Note that the transformation is shallow: In @D (C True) 3@, the
-- inner value @C True@ of type @A@ is not affected by the
-- transformation.
--
-- For more details about @'Rep' A@, have a look at the
-- "Generics.SOP.Universe" module.
--
-- == Defining a generic function
--
-- As an example of a generic function, let us define a generic
-- version of 'Control.DeepSeq.rnf' from the @deepseq@ package.
--
-- The type of 'Control.DeepSeq.rnf' is
--
-- @
-- NFData a => a -> ()
-- @
--
-- and the idea is that for a term @x@ of type @a@ in the
-- 'Control.DeepSeq.NFData' class, @rnf x@ forces complete evaluation
-- of @x@ (i.e., evaluation to /normal form/), and returns @()@.
--
-- We call the generic version of this function @grnf@. A direct
-- definition in SOP style, making use of structural recursion on the
-- sums and products, looks as follows:
--
-- @
-- grnf :: ('Generic' a, 'All2' NFData ('Code' a)) => a -> ()
-- grnf x = grnfS ('from' x)
--
-- grnfS :: ('All2' NFData xss) => 'SOP' 'I' xss -> ()
-- grnfS ('SOP' ('Z' xs))  = grnfP xs
-- grnfS ('SOP' ('S' xss)) = grnfS ('SOP' xss)
--
-- grnfP :: ('All' NFData xs) => 'NP' 'I' xs -> ()
-- grnfP 'Nil'         = ()
-- grnfP ('I' x ':*' xs) = x \`deepseq\` (grnfP xs)
-- @
--
-- The @grnf@ function performs the conversion between @a@ and @'Rep' a@
-- by applying 'from' and then applies @grnfS@. The type of @grnf@
-- indicates that @a@ must be in the 'Generic' class so that we can
-- apply 'from', and that all the components of @a@ (i.e., all the types
-- that occur as constructor arguments) must be in the 'NFData' class
-- ('All2').
--
-- The function @grnfS@ traverses the outer sum structure of the
-- sum of products (note that @'Rep' a = 'SOP' 'I' ('Code' a)@). It
-- encodes which constructor was used to construct the original
-- argument of type @a@. Once we've found the constructor in question
-- ('Z'), we traverse the arguments of that constructor using @grnfP@.
--
-- The function @grnfP@ traverses the product structure of the
-- constructor arguments. Each argument is evaluated using the
-- 'Control.DeepSeq.deepseq' function from the 'Control.DeepSeq.NFData'
-- class. This requires that all components of the product must be
-- in the 'NFData' class ('All') and triggers the corresponding
-- constraints on the other functions. Once the end of the product
-- is reached ('Nil'), we return @()@.
--
-- == Defining a generic function using combinators
--
-- In many cases, generic functions can be written in a much more
-- concise way by avoiding the explicit structural recursion and
-- resorting to the powerful combinators provided by this library
-- instead.
--
-- For example, the @grnf@ function can also be defined as a one-liner
-- as follows:
--
-- @
-- grnf :: ('Generic' a, 'All2' NFData ('Code' a)) => a -> ()
-- grnf = 'rnf' . 'hcollapse' . 'hcmap' ('Proxy' :: 'Proxy' NFData) ('mapIK' rnf) . 'from'
-- @
--
-- 'mapIK' and friends ('mapII', 'mapKI', etc.) are small helpers for working
-- with 'I' and 'K' functors, for example 'mapIK' is defined as
-- @'mapIK' f = \\ ('I' x) -> 'K' (f x)@
--
-- The following interaction should provide an idea of the individual
-- transformation steps:
--
-- >>> let x = G 2.5 'A' False :: B Double
-- >>> from x
-- SOP (S (Z (I 2.5 :* I 'A' :* I False :* Nil)))
-- >>> hcmap (Proxy :: Proxy NFData) (mapIK rnf) it
-- SOP (S (Z (K () :* K () :* K () :* Nil)))
-- >>> hcollapse it
-- [(),(),()]
-- >>> rnf it
-- ()
--
-- The 'from' call converts into the structural representation.
-- Via 'hcmap', we apply 'rnf' to all the components. The result
-- is a sum of products of the same shape, but the components are
-- no longer heterogeneous ('I'), but homogeneous (@'K' ()@). A
-- homogeneous structure can be collapsed ('hcollapse') into a
-- normal Haskell list. Finally, 'rnf' actually forces evaluation
-- of this list (and thereby actually drives the evaluation of all
-- the previous steps) and produces the final result.
--
-- == Using a generic function
--
-- We can directly invoke 'grnf' on any type that is an instance of
-- class 'Generic'.
--
-- >>> grnf (G 2.5 'A' False)
-- ()
-- >>> grnf (G 2.5 undefined False)
-- *** Exception: Prelude.undefined
-- ...
--
-- Note that the type of 'grnf' requires that all components of the
-- type are in the 'Control.DeepSeq.NFData' class. For a recursive
-- datatype such as @B@, this means that we have to make @A@
-- (and in this case, also @B@) an instance of 'Control.DeepSeq.NFData'
-- in order to be able to use the 'grnf' function. But we can use 'grnf'
-- to supply the instance definitions:
--
-- > instance NFData A where rnf = grnf
-- > instance NFData a => NFData (B a) where rnf = grnf
--
-- = More examples
--
-- The best way to learn about how to define generic functions in the SOP style
-- is to look at a few simple examples. Examples are provided by the following
-- packages:
--
--   * @<http://hackage.haskell.org/package/basic-sop basic-sop>@ basic examples,
--   * @<http://hackage.haskell.org/package/pretty-sop pretty-sop>@ generic pretty printing,
--   * @<http://hackage.haskell.org/package/lens-sop lens-sop>@ generically computed lenses,
--   * @<http://hackage.haskell.org/package/json-sop json-sop>@ generic JSON conversions.
--
-- The generic functions in these packages use a wide variety of the combinators
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
  , IsProductType
  , ProductCode
  , productTypeFrom
  , productTypeTo
  , IsSumType
  , SumCode
  , sumTypeFrom
  , sumTypeTo
  , IsEnumType
  , enumTypeFrom
  , enumTypeTo
  , IsWrappedType
  , WrappedCode
  , wrappedTypeFrom
  , wrappedTypeTo
  , IsNewtype
  , newtypeFrom
  , newtypeTo
    -- * n-ary datatypes
  , NP(..)
  , NS(..)
  , SOP(..)
  , unSOP
  , POP(..)
  , unPOP
    -- * Metadata
  , DatatypeInfo(..)
  , moduleName
  , datatypeName
  , constructorInfo
  , ConstructorInfo(..)
  , constructorName
  , FieldInfo(..)
  , fieldName
  , HasDatatypeInfo(..)
  , DatatypeName
  , ModuleName
  , ConstructorName
  , FieldName
  , Associativity(..)
  , Fixity
    -- * Combinators
    -- ** Constructing products
  , HPure(..)
    -- ** Destructing products
  , hd
  , tl
  , Projection
  , projections
  , shiftProjection
    -- ** Application
  , type (-.->)(..)
  , fn
  , fn_2
  , fn_3
  , fn_4
  , Prod
  , HAp(..)
    -- ** Lifting / mapping
  , hliftA
  , hliftA2
  , hliftA3
  , hcliftA
  , hcliftA2
  , hcliftA3
  , hmap
  , hzipWith
  , hzipWith3
  , hcmap
  , hczipWith
  , hczipWith3
    -- ** Constructing sums
  , Injection
  , injections
  , shift
  , shiftInjection
  , UnProd
  , HApInjs(..)
  , apInjs_NP  -- deprecated export
  , apInjs_POP -- deprecated export
    -- ** Destructing sums
  , unZ
  , HIndex(..)
  , Ejection
  , ejections
  , shiftEjection
    -- ** Dealing with @'All' c@
  , hcliftA'
  , hcliftA2'
  , hcliftA3'
    -- ** Comparison
  , compare_NS
  , ccompare_NS
  , compare_SOP
  , ccompare_SOP
    -- ** Collapsing
  , CollapseTo
  , HCollapse(..)
    -- ** Folding and sequencing
  , HTraverse_(..)
  , hcfoldMap
  , hcfor_
  , HSequence(..)
  , hsequence
  , hsequenceK
  , hctraverse
  , hcfor
    -- ** Expanding sums to products
  , HExpand(..)
    -- ** Transformation of index lists and coercions
  , HTrans(..)
  , hfromI
  , htoI
    -- ** Partial operations
  , fromList
    -- * Utilities
    -- ** Basic functors
  , K(..)
  , unK
  , I(..)
  , unI
  , (:.:)(..)
  , unComp
    -- *** Mapping functions
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
    -- ** Mapping constraints
  , All
  , All2
  , cpara_SList
  , ccase_SList
  , AllZip
  , AllZip2
  , AllN
  , AllZipN
    -- ** Other constraints
  , Compose
  , And
  , Top
  , LiftedCoercible
  , SameShapeAs
    -- ** Singletons
  , SList(..)
  , SListI
  , SListI2
  , sList
  , para_SList
  , case_SList
    -- *** Shape of type-level lists
  , Shape(..)
  , shape
  , lengthSList
    -- ** Re-exports

-- Workaround for lack of MIN_TOOL_VERSION macro in Cabal 1.18, see:
-- https://github.com/well-typed/generics-sop/issues/3
#ifndef MIN_TOOL_VERSION_haddock
#define MIN_TOOL_VERSION_haddock(x,y,z) 0
#endif

#if !(defined(__HADDOCK_VERSION__)) || MIN_TOOL_VERSION_haddock(2,14,0)
  , Proxy(..) -- hidden from old Haddock versions, because it triggers an internal error
#endif
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

-- $setup
--
-- >>> :set -XDeriveGeneric
-- >>> import qualified GHC.Generics as GHC
-- >>> import Generics.SOP
-- >>> import Control.DeepSeq
-- >>> data B a = F | G a Char Bool deriving (Show, GHC.Generic)
-- >>> data A   = C Bool | D A Int | E (B ()) deriving (Show, GHC.Generic)
-- >>> instance Generic A     -- empty
-- >>> instance Generic (B a) -- empty
--
-- >>> let grnf = rnf . hcollapse . hcmap (Proxy :: Proxy NFData) (\ (I x) -> K (rnf x)) . from
