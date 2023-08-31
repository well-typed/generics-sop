{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -freduction-depth=100 #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}
-- | Instances for 'Generic' and 'HasMetadata'.
--
-- We define instances for datatypes from @generics-sop@ and
-- @base@ that are supported.
--
-- (There are only instances defined in this module, so the
-- documentation is empty.)
--
module Generics.SOP.Instances () where

-- GHC versions and base versions:
--
-- 7.6.3:  4.6.0.1
-- 7.8.3:  4.7.0.1
-- 7.8.4:  4.7.0.2
-- 7.10.3: 4.8.2.0
-- 8.0.2:  4.9.1.0
-- 8.2.2:  4.10.1.0
-- 8.4.3:  4.11.1.0
-- 8.6.1:  4.12.0.0

import Control.Exception
import Data.Char
import Data.Complex
import Data.Data
import Data.Fixed
import Data.Functor.Compose -- new
import qualified Data.Functor.Const -- new
import Data.Functor.Identity -- new
import Data.Functor.Product -- new
import Data.Functor.Sum -- new
import Data.List.NonEmpty -- new
import qualified Data.Monoid
import Data.Ord
import qualified Data.Semigroup -- new
import Data.Version
import Data.Void -- new
import Foreign.C.Error
import Foreign.C.Types
#if MIN_VERSION_base(4,11,0)
import GHC.ByteOrder -- new
#endif
import GHC.Conc -- new
import GHC.ExecutionStack -- new
import GHC.Exts -- new
-- import GHC.Events -- platform-specific, omitted
import GHC.Fingerprint -- new
import GHC.Float -- new
import qualified GHC.Generics -- new
import GHC.IO.Buffer -- new
import GHC.IO.Device -- new
import GHC.IO.Encoding -- new
import GHC.IO.Encoding.Failure -- new
import GHC.IO.Exception -- new
import GHC.IO.Handle -- new
import GHC.RTS.Flags -- new
import qualified GHC.Stack -- new
import GHC.StaticPtr -- new
import GHC.Stats -- new
import System.Console.GetOpt
import System.IO
import Text.Printf
import Text.Read.Lex

import Generics.SOP.BasicFunctors
import Generics.SOP.Classes
import Generics.SOP.TH
#if MIN_VERSION_base(4,15,0)
import GHC.Tuple (Solo)
#endif

-- Types from Generics.SOP:

deriveGeneric ''I
deriveGeneric ''K
deriveGeneric ''(:.:)
deriveGeneric ''(-.->) -- new

-- Cannot derive instances for Sing
-- Cannot derive instances for Shape
-- Cannot derive instances for NP, NS, POP, SOP
-- Cannot derive instances for metadata types

-- Types from the Prelude:

deriveGeneric ''Bool
deriveGeneric ''Ordering
deriveGeneric ''Maybe
deriveGeneric ''Either
deriveGeneric ''()
#if MIN_VERSION_base(4,15,0)
deriveGeneric ''Solo
#endif
deriveGeneric ''(,)              -- 2
deriveGeneric ''(,,)
deriveGeneric ''(,,,)
deriveGeneric ''(,,,,)           -- 5
deriveGeneric ''(,,,,,)
deriveGeneric ''(,,,,,,)
deriveGeneric ''(,,,,,,,)
deriveGeneric ''(,,,,,,,,)
deriveGeneric ''(,,,,,,,,,)      -- 10
deriveGeneric ''(,,,,,,,,,,)
deriveGeneric ''(,,,,,,,,,,,)
deriveGeneric ''(,,,,,,,,,,,,)
deriveGeneric ''(,,,,,,,,,,,,,)
deriveGeneric ''(,,,,,,,,,,,,,,) -- 15
deriveGeneric ''(,,,,,,,,,,,,,,,)
deriveGeneric ''(,,,,,,,,,,,,,,,,)
deriveGeneric ''(,,,,,,,,,,,,,,,,,)
deriveGeneric ''(,,,,,,,,,,,,,,,,,,)
deriveGeneric ''(,,,,,,,,,,,,,,,,,,,) -- 20
deriveGeneric ''(,,,,,,,,,,,,,,,,,,,,)
deriveGeneric ''(,,,,,,,,,,,,,,,,,,,,,)
deriveGeneric ''(,,,,,,,,,,,,,,,,,,,,,,)
deriveGeneric ''(,,,,,,,,,,,,,,,,,,,,,,,)
deriveGeneric ''(,,,,,,,,,,,,,,,,,,,,,,,,) -- 25
deriveGeneric ''(,,,,,,,,,,,,,,,,,,,,,,,,,)
deriveGeneric ''(,,,,,,,,,,,,,,,,,,,,,,,,,,)
deriveGeneric ''(,,,,,,,,,,,,,,,,,,,,,,,,,,,)
deriveGeneric ''(,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
deriveGeneric ''(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) -- 30
deriveGeneric ''[]

-- Other types from base:

-- From Control.Exception:
deriveGeneric ''IOException
deriveGeneric ''ArithException
deriveGeneric ''ArrayException
deriveGeneric ''AssertionFailed
deriveGeneric ''AsyncException
deriveGeneric ''NonTermination
deriveGeneric ''NestedAtomically
deriveGeneric ''BlockedIndefinitelyOnMVar
deriveGeneric ''BlockedIndefinitelyOnSTM
deriveGeneric ''AllocationLimitExceeded -- new
deriveGeneric ''Deadlock
deriveGeneric ''NoMethodError
deriveGeneric ''PatternMatchFail
deriveGeneric ''RecConError
deriveGeneric ''RecSelError
deriveGeneric ''RecUpdError
deriveGeneric ''ErrorCall
deriveGeneric ''TypeError -- new
deriveGeneric ''MaskingState

-- From Data.Char:
deriveGeneric ''GeneralCategory

-- From Data.Complex:
deriveGeneric ''Complex

-- From Data.Data:
deriveGeneric ''DataRep
deriveGeneric ''Fixity
deriveGeneric ''ConstrRep

-- From Data.Fixed:
deriveGeneric ''Fixed
deriveGeneric ''E0
deriveGeneric ''E1
deriveGeneric ''E2
deriveGeneric ''E3
deriveGeneric ''E6
deriveGeneric ''E9
deriveGeneric ''E12

-- From Data.Functor.Compose
deriveGeneric ''Compose -- new

-- From Data.Functor.Const
deriveGeneric ''Data.Functor.Const.Const -- new

-- From Data.Functor.Identity
deriveGeneric ''Identity -- new

-- From Data.Functor.Product
deriveGeneric ''Product -- new

-- From Data.Functor.Sum
deriveGeneric ''Sum -- new

-- From Data.List.NonEmpty
deriveGeneric ''NonEmpty -- new

-- From Data.Monoid:
deriveGeneric ''Data.Monoid.Dual
deriveGeneric ''Data.Monoid.Endo
deriveGeneric ''Data.Monoid.All
deriveGeneric ''Data.Monoid.Any
deriveGeneric ''Data.Monoid.Sum
deriveGeneric ''Data.Monoid.Product
deriveGeneric ''Data.Monoid.First
deriveGeneric ''Data.Monoid.Last
deriveGeneric ''Data.Monoid.Alt -- new

-- From Data.Ord:
deriveGeneric ''Down

-- From Data.Proxy:
deriveGeneric ''Proxy

-- From Data.Semigroup:
deriveGeneric ''Data.Semigroup.Min -- new
deriveGeneric ''Data.Semigroup.Max -- new
deriveGeneric ''Data.Semigroup.First -- new
deriveGeneric ''Data.Semigroup.Last -- new
deriveGeneric ''Data.Semigroup.WrappedMonoid -- new
#if !MIN_VERSION_base(4,16,0)
deriveGeneric ''Data.Semigroup.Option -- new
#endif
deriveGeneric ''Data.Semigroup.Arg -- new

-- From Data.Version:
deriveGeneric ''Version

-- From Data.Void:
deriveGeneric ''Void -- new

-- From Foreign.C.Error:
deriveGeneric ''Errno

-- From Foreign.C.Types:
deriveGeneric ''CChar
deriveGeneric ''CSChar
deriveGeneric ''CUChar
deriveGeneric ''CShort
deriveGeneric ''CUShort
deriveGeneric ''CInt
deriveGeneric ''CUInt
deriveGeneric ''CLong
deriveGeneric ''CULong
deriveGeneric ''CPtrdiff
deriveGeneric ''CSize
deriveGeneric ''CWchar
deriveGeneric ''CSigAtomic
deriveGeneric ''CLLong
deriveGeneric ''CULLong
deriveGeneric ''CIntPtr
deriveGeneric ''CUIntPtr
deriveGeneric ''CIntMax
deriveGeneric ''CUIntMax
deriveGeneric ''CClock
deriveGeneric ''CTime
deriveGeneric ''CUSeconds
deriveGeneric ''CSUSeconds
deriveGeneric ''CFloat
deriveGeneric ''CDouble

#if MIN_VERSION_base(4,11,0)
-- From GHC.ByteOrder:
deriveGeneric ''ByteOrder -- new
#endif

-- From GHC.Conc:
deriveGeneric ''ThreadStatus -- new
deriveGeneric ''BlockReason -- new

-- From GHC.ExecutionStack:
deriveGeneric ''Location -- new
deriveGeneric ''SrcLoc -- new

-- From GHC.Exts:
deriveGeneric ''RuntimeRep -- new
deriveGeneric ''VecCount -- new
deriveGeneric ''VecElem -- new
#if !MIN_VERSION_base(4,15,0)
deriveGeneric ''SpecConstrAnnotation -- new
#endif

-- From GHC.Generics:
deriveGeneric ''GHC.Generics.K1 -- new
deriveGeneric ''GHC.Generics.U1 -- new
deriveGeneric ''GHC.Generics.V1 -- new
deriveGeneric ''GHC.Generics.Par1 -- new
deriveGeneric ''GHC.Generics.M1 -- new
deriveGeneric ''GHC.Generics.R -- new
deriveGeneric ''GHC.Generics.S -- new
deriveGeneric ''GHC.Generics.D -- new
deriveGeneric ''GHC.Generics.C -- new
deriveGeneric ''(GHC.Generics.:*:) -- new
deriveGeneric ''(GHC.Generics.:+:) -- new
deriveGeneric ''(GHC.Generics.:.:) -- new
deriveGeneric ''GHC.Generics.Associativity -- new
deriveGeneric ''GHC.Generics.DecidedStrictness -- new
deriveGeneric ''GHC.Generics.SourceStrictness -- new
deriveGeneric ''GHC.Generics.SourceUnpackedness -- new
deriveGeneric ''GHC.Generics.Fixity -- new

-- From GHC.IO.Buffer:
deriveGeneric ''Buffer -- new
deriveGeneric ''BufferState -- new

-- From GHC.IO.Device:
deriveGeneric ''IODeviceType -- new

-- From GHC.IO.Encoding:
deriveGeneric ''BufferCodec -- new
deriveGeneric ''CodingProgress -- new

-- From GHC.IO.Encoding.Failure:
deriveGeneric ''CodingFailureMode -- new

-- From GHC.Fingerprint
deriveGeneric ''Fingerprint -- new

-- From GHC.Float
deriveGeneric ''FFFormat -- new

-- From GHC.IO.Exception:
#if MIN_VERSION_base(4,11,0)
deriveGeneric ''FixIOException -- new
deriveGeneric ''IOErrorType -- new
#endif

-- From GHC.IO.Handle:
deriveGeneric ''HandlePosn -- new
#if MIN_VERSION_base(4,10,0)
deriveGeneric ''LockMode -- new
#endif

-- From GHC.RTS.Flags:
deriveGeneric ''RTSFlags -- new
deriveGeneric ''GiveGCStats -- new
deriveGeneric ''GCFlags -- new
deriveGeneric ''ConcFlags -- new
deriveGeneric ''MiscFlags -- new
deriveGeneric ''DebugFlags -- new
deriveGeneric ''DoCostCentres -- new
deriveGeneric ''CCFlags -- new
deriveGeneric ''DoHeapProfile -- new
deriveGeneric ''ProfFlags -- new
deriveGeneric ''DoTrace -- new
deriveGeneric ''TraceFlags -- new
deriveGeneric ''TickyFlags -- new
#if MIN_VERSION_base(4,10,0)
deriveGeneric ''ParFlags -- new
#endif

-- From GHC.Stack:
deriveGeneric ''GHC.Stack.SrcLoc -- new
deriveGeneric ''GHC.Stack.CallStack -- new

-- From GHC.StaticPtr:
deriveGeneric ''StaticPtrInfo -- new

-- From GHC.Stats:
#if MIN_VERSION_base(4,10,0)
deriveGeneric ''RTSStats -- new
deriveGeneric ''GCDetails -- new
#endif
#if !MIN_VERSION_base(4,11,0)
deriveGeneric ''GCStats -- new
#endif

-- From System.Console.GetOpt:

deriveGeneric ''ArgOrder
deriveGeneric ''OptDescr
deriveGeneric ''ArgDescr

-- From System.Exit:

deriveGeneric ''ExitCode

-- From System.IO:

deriveGeneric ''IOMode
deriveGeneric ''BufferMode
deriveGeneric ''SeekMode
deriveGeneric ''Newline
deriveGeneric ''NewlineMode

-- From Text.Printf:

deriveGeneric ''FieldFormat
deriveGeneric ''FormatAdjustment
deriveGeneric ''FormatSign
deriveGeneric ''FormatParse

-- From Text.Read.Lex:

deriveGeneric ''Lexeme
deriveGeneric ''Number

-- Abstract / primitive datatypes (we don't derive Generic for these):
--
-- Ratio
-- Integer
-- ThreadId
-- Chan
-- MVar
-- QSem
-- QSemN
-- DataType
-- Dynamic
-- IORef
-- TypeRep
-- TyCon
-- TypeRepKey
-- KProxy -- not abstract, but intended for kind-level use
-- STRef
-- Unique
-- ForeignPtr
-- CFile
-- CFpos
-- CJmpBuf
-- Pool
-- Ptr
-- FunPtr
-- IntPtr
-- WordPtr
-- StablePtr
-- Char
-- Double
-- Float
-- Int
-- Int8
-- Int16
-- Int32
-- Int64
-- Word
-- Word8
-- Word16
-- Word32
-- Word64
-- IO
-- ST
-- (->)
-- RealWorld
-- Handle
-- HandlePosn
-- TextEncoding
-- StableName
-- Weak
-- ReadP
-- ReadPrec
-- STM
-- TVar
-- Natural
-- Event
-- EventManager
-- CostCentre
-- CostCentreStack
--
-- Datatypes we cannot currently handle:
--
-- SomeException
-- SomeAsyncException
-- Handler
-- Coercion
-- (:~:)
