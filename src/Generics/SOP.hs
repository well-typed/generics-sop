{-# LANGUAGE PolyKinds, UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
-- | Sum-of-products generic universe
--
-- In the source of this module we provide a bunch of specialized functions
-- for type class methods; this is to keep the module in line with the paper.
-- We don't export these.
module Generics.SOP (
    -- * Basic functors
    K(..)
  , I(..)
  , (:.:)(..)
    -- * Codes and interpretations
  , Code
  , Rep
  , Generic(..)
    -- * n-ary datatypes
  , NP(..)
  , NS(..)
  , SOP(..)
  , POP(..)
    -- * Singletons
  , Sing(..)
  , SingI(..)
    -- * Mapping constraints
  , All
  , All2
  , Map
  , AllMap
    -- * Constructing products
  , HPure(..)
    -- * Application
  , (-.->)(..)
  , fn
  , fn_2
  , fn_3
  , fn_4
  , Prod
  , HAp(..)
    -- * Derived application
  , hliftA
  , hliftA2
  , hliftA3
  , hcliftA
  , hcliftA2
  , hcliftA3
    -- * Constructing sums
  , Injection
  , injections
  , shift
  , apInjs_NP
  , apInjs_POP
    -- * Dealing with 'All c'
  , AllDict(..)
  , allDict_NP
  , hcliftA'
  , hcliftA2'
  , hcliftA3'
    -- * Collapsing
  , CollapseTo
  , HCollapse(..)
    -- * Sequencing
  , HSequence(..)
  , hsequence
  , hsequenceK
    -- * Partial operations
  , fromList
    -- * Metadata
  , DatatypeName
  , ModuleName
  , ConstructorName
  , FieldName
  , DatatypeInfo(..)
  , ConstructorInfo(..)
  , FieldInfo(..)
  , HasDatatypeInfo(..)
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
