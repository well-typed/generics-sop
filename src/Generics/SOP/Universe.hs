{-# LANGUAGE UndecidableInstances #-}
#if MIN_VERSION_base(4,7,0)
{-# LANGUAGE AutoDeriveTypeable #-}
#endif
-- | Codes and interpretations
module Generics.SOP.Universe where

import Data.Proxy
import qualified GHC.Generics as GHC

import Generics.SOP.BasicFunctors
import Generics.SOP.NS
import Generics.SOP.Sing
import Generics.SOP.GGP
import Generics.SOP.Metadata

-- | The (generic) representation of a datatype.
--
-- A datatype is isomorphic to the sum-of-products of its code.
-- The isomorphism is witnessed by 'from' and 'to' from the
-- 'Generic' class.
--
type Rep a = SOP I (Code a)

-- | The class of representable datatypes.
--
-- The SOP approach to generic programming is based on viewing
-- datatypes as a representation ('Rep') built from the sum of
-- products of its components. The components of are datatype
-- are specified using the 'Code' type family.
--
-- The isomorphism between the original Haskell datatype and its
-- representation is witnessed by the methods of this class,
-- 'from' and 'to'. So for instances of this class, the following
-- laws should (in general) hold:
--
-- > to . from === id :: a -> a
-- > from . to === id :: Rep a -> Rep a
--
-- You typically don't define instances of this class by hand, but
-- rather derive the class instance automatically.
--
-- /Option 1:/ Derive via the built-in GHC-generics. For this, you
-- need to use the @DeriveGeneric@ extension to first derive an
-- instance of the 'GHC.Generics.Generic' class from module "GHC.Generics".
-- With this, you can then give an empty instance for 'Generic', and
-- the default definitions will just work. The pattern looks as
-- follows:
--
-- > import qualified GHC.Generics as GHC
-- > import Generics.SOP
-- >
-- > ...
-- >
-- > data T = ... deriving (GHC.Generic, ...)
-- >
-- > instance Generic T -- empty
-- > instance HasMetadata T -- empty, if you want/need metadata
--
-- /Option 2:/ Derive via Template Haskell. For this, you need to
-- enable the @TemplateHaskell@ extension. You can then use
-- 'Generics.SOP.TH.deriveGeneric' from module "Generics.SOP.TH"
-- to have the instance generated for you. The pattern looks as
-- follows:
--
-- > import Generics.SOP
-- > import Generics.SOP.TH
-- >
-- > ...
-- >
-- > data T = ...
-- >
-- > deriveGeneric ''T -- derives HasMetadata as well
--
-- /Tradeoffs:/ Whether to use Option 1 or 2 is mainly a matter
-- of personal taste. If you like Template Haskell, then this
-- version has probably less run-time overhead.
--
-- /Non-standard instances:/
-- It is possible to give 'Generic' instances manually that deviate
-- from the standard scheme, as long as at least
--
-- > to . from === id :: a -> a
--
-- still holds.
--
class SingI (Code a) => Generic (a :: *) where
  -- | The code of a datatype.
  --
  -- This is a list of lists of its components. The outer list contains
  -- one element per constructor. The inner list contains one element
  -- per constructor argument (field).
  --
  -- /Example:/ The datatype
  --
  -- > data Tree = Leaf Int | Node Tree Tree
  --
  -- is supposed to have the following code:
  --
  -- > type instance Code (Tree a) =
  -- >   '[ '[ Int ]
  -- >    , '[ Tree, Tree ]
  -- >    ]
  --
  type Code a :: [[*]]
  type Code a = GCode a

  -- | Converts from a value to its structural representation.
  from         :: a -> Rep a
  default from :: (GFrom a, GHC.Generic a) => a -> SOP I (GCode a)
  from = gfrom

  -- | Converts from a structural representation back to the
  -- original value.
  to         :: Rep a -> a
  default to :: (GTo a, GHC.Generic a) => SOP I (GCode a) -> a
  to = gto

-- | A class of datatypes that have associated metadata.
--
-- It is possible to use the sum-of-products approach to generic programming
-- without metadata. If you need metadata in a function, an additional
-- constraint on this class is in order.
--
-- You typically don't define instances of this class by hand, but
-- rather derive the class instance automatically. See the documentation
-- of 'Generic' for the options.
--
class HasDatatypeInfo a where
  datatypeInfo         :: Proxy a -> DatatypeInfo (Code a)
  default datatypeInfo :: (GDatatypeInfo a) => Proxy a -> DatatypeInfo (GCode a)
  datatypeInfo = gdatatypeInfo
