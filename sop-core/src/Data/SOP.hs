{-# LANGUAGE PolyKinds, UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
-- | Main module of @sop-core@
module Data.SOP (
    -- * n-ary datatypes
    NP(..)
  , NS(..)
  , SOP(..)
  , unSOP
  , POP(..)
  , unPOP
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

import Data.SOP.BasicFunctors
import Data.SOP.Classes
import Data.SOP.Constraint
import Data.SOP.NP
import Data.SOP.NS
import Data.SOP.Sing

