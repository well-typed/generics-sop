-- | Codes and interpretations
module Generics.SOP.Universe where

import Generics.SOP.BasicFunctors
import Generics.SOP.NS
import Generics.SOP.Sing

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
-- You can generate 'Code' instances using Template Haskell via
-- 'Generics.SOP.TH.deriveGeneric'. It's possible to give 'Code'
-- instances manually that deviate from the standard scheme if the
-- 'Generic' instance is defined accordingly.
--
type family Code a :: [[*]]

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
-- rather use 'Generics.SOP.TH.deriveGeneric' and Template Haskell
-- to generate an instance for you automatically. It is, however,
-- possible to give 'Generic' instances manually that deviate from
-- the standard scheme, as long as at least
--
-- > to . from === id :: a -> a
--
-- still holds.
--
class SingI (Code a) => Generic (a :: *) where
  from :: a -> Rep a
  to   :: Rep a -> a

