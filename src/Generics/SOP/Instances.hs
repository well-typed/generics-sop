{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fcontext-stack=50 #-}
-- | Instances for 'Generic' and 'HasMetadata'.
--
-- We define instances for datatypes from @generics-sop@ and
-- @base@ that are supported.
--
-- (There are only instances defined in this module, so the
-- documentation is empty.)
--
module Generics.SOP.Instances () where

import Control.Exception
import Data.Char
import Data.Complex
import Data.Data
import Data.Fixed
import Data.Monoid
import Data.Ord
#if !(MIN_VERSION_base(4,7,0))
import Data.Proxy
#endif
import Data.Version
import Foreign.C.Error
import Foreign.C.Types
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
deriveGeneric ''Deadlock
deriveGeneric ''NoMethodError
deriveGeneric ''PatternMatchFail
deriveGeneric ''RecConError
deriveGeneric ''RecSelError
deriveGeneric ''RecUpdError
deriveGeneric ''ErrorCall
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

-- From Data.Monoid:
deriveGeneric ''Dual
deriveGeneric ''Endo
deriveGeneric ''All
deriveGeneric ''Any
deriveGeneric ''Sum
deriveGeneric ''Product
deriveGeneric ''First
deriveGeneric ''Last

-- From Data.Ord:
deriveGeneric ''Down

-- From Data.Proxy:
deriveGeneric ''Proxy

-- From Data.Version:
deriveGeneric ''Version

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
--
-- Datatypes we cannot currently handle:
--
-- SomeException
-- SomeAsyncException
-- Handler
-- Coercion
-- (:~:)
