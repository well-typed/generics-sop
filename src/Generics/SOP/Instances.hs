{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -freduction-depth=100 #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
#else
{-# OPTIONS_GHC -fcontext-stack=50 #-}
#endif
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
-- 8.2.1:  4.10.?.?

import Control.Applicative -- new
import Control.Arrow -- new
import Control.Exception
import Data.Char
import Data.Complex
import Data.Data
import Data.Fixed
#if MIN_VERSION_base(4,9,0)
import Data.Functor.Compose -- new
import qualified Data.Functor.Const -- new
#endif
#if MIN_VERSION_base(4,8,0)
import Data.Functor.Identity -- new
#endif
#if MIN_VERSION_base(4,9,0)
import Data.Functor.Product -- new
import Data.Functor.Sum -- new
import Data.List.NonEmpty -- new
#endif
import qualified Data.Monoid
import Data.Ord
#if !(MIN_VERSION_base(4,7,0))
import Data.Proxy
#endif
#if MIN_VERSION_base(4,9,0)
import qualified Data.Semigroup -- new
#endif
import Data.Typeable.Internal -- new
import Data.Version
#if MIN_VERSION_base(4,8,0)
import Data.Void -- new
#endif
import Foreign.C.Error
import Foreign.C.Types
import GHC.Conc -- new
#if MIN_VERSION_base(4,9,0)
import GHC.ExecutionStack -- new
#endif
import GHC.Exts -- new
import GHC.IO.Buffer -- new
import GHC.IO.Device -- new
import GHC.IO.Encoding -- new
import GHC.IO.Encoding.Failure -- new
import GHC.IO.Handle -- new
#if MIN_VERSION_base(4,8,0)
import GHC.RTS.Flags -- new
#endif
#if MIN_VERSION_base(4,9,0)
import qualified GHC.Stack -- new
#endif
#if MIN_VERSION_base(4,8,0)
import GHC.StaticPtr -- new
#endif
import GHC.Stats -- new
import System.Console.GetOpt
import System.Exit
import System.IO
#if MIN_VERSION_base(4,7,0)
import Text.Printf
#endif
import Text.Read.Lex

import Generics.SOP.BasicFunctors
import Generics.SOP.TH

-- Types from Generics.SOP:

deriveGeneric ''I
deriveGeneric ''K
deriveGeneric ''(:.:)

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

-- From Control.Applicative:
deriveGeneric ''WrappedMonad -- new
deriveGeneric ''WrappedArrow -- new
deriveGeneric ''ZipList -- new

-- From Control.Arrow:
deriveGeneric ''Kleisli -- new
deriveGeneric ''ArrowMonad -- new

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
#if MIN_VERSION_base(4,8,0)
deriveGeneric ''AllocationLimitExceeded -- new
#endif
deriveGeneric ''Deadlock
deriveGeneric ''NoMethodError
deriveGeneric ''PatternMatchFail
deriveGeneric ''RecConError
deriveGeneric ''RecSelError
deriveGeneric ''RecUpdError
deriveGeneric ''ErrorCall
#if MIN_VERSION_base(4,9,0)
deriveGeneric ''TypeError -- new
#endif
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
-- TODO: What about empty types? E0, E1, ..., E12

-- From Data.Functor.Compose
#if MIN_VERSION_base(4,9,0)
deriveGeneric ''Compose -- new
#endif

-- From Data.Functor.Const
#if MIN_VERSION_base(4,9,0)
deriveGeneric ''Data.Functor.Const.Const -- new
#endif

-- From Data.Functor.Identity
#if MIN_VERSION_base(4,8,0)
deriveGeneric ''Identity -- new
#endif

-- From Data.Functor.Product
#if MIN_VERSION_base(4,9,0)
deriveGeneric ''Product -- new
#endif

-- From Data.Functor.Sum
#if MIN_VERSION_base(4,9,0)
deriveGeneric ''Sum -- new
#endif

-- From Data.List.NonEmpty
#if MIN_VERSION_base(4,9,0)
deriveGeneric ''NonEmpty -- new
#endif

-- From Data.Monoid:
deriveGeneric ''Data.Monoid.Dual
deriveGeneric ''Data.Monoid.Endo
deriveGeneric ''Data.Monoid.All
deriveGeneric ''Data.Monoid.Any
deriveGeneric ''Data.Monoid.Sum
deriveGeneric ''Data.Monoid.Product
deriveGeneric ''Data.Monoid.First
deriveGeneric ''Data.Monoid.Last
#if MIN_VERSION_base(4,8,0)
deriveGeneric ''Data.Monoid.Alt -- new
#endif

-- From Data.Ord:
deriveGeneric ''Down

-- From Data.Proxy:
deriveGeneric ''Proxy

-- From Data.Semigroup:
#if MIN_VERSION_base(4,9,0)
deriveGeneric ''Data.Semigroup.Min -- new
deriveGeneric ''Data.Semigroup.Max -- new
deriveGeneric ''Data.Semigroup.First -- new
deriveGeneric ''Data.Semigroup.Last -- new
deriveGeneric ''Data.Semigroup.WrappedMonoid -- new
deriveGeneric ''Data.Semigroup.Option -- new
deriveGeneric ''Data.Semigroup.Arg -- new
#endif

-- From Data.Typeable.Internal
deriveGeneric ''Fingerprint -- new

-- From Data.Version:
deriveGeneric ''Version

-- From Data.Void:
#if MIN_VERSION_base(4,8,0)
-- TODO: Another empty type
-- deriveGeneric ''Void -- new
#endif

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

-- From GHC.Conc:
deriveGeneric ''ThreadStatus -- new
deriveGeneric ''BlockReason -- new

-- From GHC.ExecutionStack:
#if MIN_VERSION_base(4,9,0)
deriveGeneric ''Location -- new
deriveGeneric ''SrcLoc -- new
#endif

-- From GHC.Exts:
#if MIN_VERSION_base(4,9,0)
deriveGeneric ''RuntimeRep -- new
deriveGeneric ''VecCount -- new
deriveGeneric ''VecElem -- new
#endif
deriveGeneric ''SpecConstrAnnotation -- new

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

-- From GHC.IO.Handle:
deriveGeneric ''HandlePosn -- new

-- From GHC.RTS.Flags:
#if MIN_VERSION_base(4,8,0)
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
#endif

-- From GHC.Stack:
#if MIN_VERSION_base(4,9,0)
deriveGeneric ''GHC.Stack.SrcLoc -- new
#endif

-- From GHC.StaticPtr:
#if MIN_VERSION_base(4,8,0)
deriveGeneric ''StaticPtrInfo -- new
#endif

-- From GHC.Stats:
deriveGeneric ''GCStats -- new

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

#if MIN_VERSION_base(4,7,0)
deriveGeneric ''FieldFormat
deriveGeneric ''FormatAdjustment
deriveGeneric ''FormatSign
deriveGeneric ''FormatParse
#endif

-- From Text.Read.Lex:

deriveGeneric ''Lexeme
#if MIN_VERSION_base(4,7,0)
deriveGeneric ''Number
#endif

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
-- Constr
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
--
-- Datatypes we cannot currently handle:
--
-- SomeException
-- SomeAsyncException
-- Handler
-- Coercion
-- (:~:)
