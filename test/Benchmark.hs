{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeFamilies     #-}
module Main where

import Control.DeepSeq
import Criterion
import Criterion.Main
import qualified GHC.Generics as GHC
import Generics.SOP
import Generics.SOP.TH


main :: IO ()
main = do
  defaultMainWith defaultConfig
    [ bgroup "GHC.Generics"
      [ bench "generic-show" $ nf gshow testGI
      , bench "show" $ nf show testGI
      ]
    , bgroup "Template Haskell"
      [ bench "generic-show" $ nf gshow testGI
      , bench "show" $ nf show testGI
      ]
    ]

-- * GShow

class GShow a where
  show_ :: a -> String

instance GShow Int where
  show_ = show

-- Generic show, kind of
gshow :: (Generic a, All2 GShow (Code a)) => a -> String
gshow x = gshowS (from x)

gshowS :: (All2 GShow xss) => SOP I xss -> String
gshowS (SOP (Z xs))  = gshowP xs
gshowS (SOP (S xss)) = gshowS (SOP xss)

gshowP :: (All GShow xs) => NP I xs -> String
gshowP Nil         = ""
gshowP (I x :* xs) = show_ x ++ (gshowP xs)

-- * List with derive generic instance

data ListGI = NilGI | ConsGI Int ListGI
  deriving (GHC.Generic, Show)

testGI :: ListGI
testGI = ConsGI 1 (ConsGI 2 (ConsGI 3 NilGI))

instance Generic ListGI
instance HasDatatypeInfo ListGI

instance NFData ListGI where
  rnf NilGI = ()
  rnf (ConsGI x ls) = rnf x `seq` rnf ls `seq` ()

instance GShow ListGI where
  show_ = gshow

-- * List with template haskell generic instance

data ListTHI = NilTHI | ConsTHI Int ListTHI
  deriving (Show)


deriveGenericFunctions ''ListTHI "ListTHITree" "fromListTHI" "toListTHI"
deriveMetadataValue ''ListTHI "ListTHITree" "listTHIDatatypeInfo"
deriveMetadataType ''ListTHI "ListTHIDatatypeInfo"

instance NFData ListTHI where
  rnf NilTHI = ()
  rnf (ConsTHI x ls) = rnf x `seq` rnf ls `seq` ()

--instance GShow ListTHI where
--  show_ = gshow

{-
Data-types:
- Parametric and non-parametric
- Flat with many constructors
- Recursive
- CoRecursive
- Mutual-recursive
- Fractal (recursive with many constructos and shapes)

General:
 - show
 - read
 - traversable

List functions:
 - map
 - filter
 - foldl
 - foldr
 - unfold

Tree-Forest functions:
 - map
 - filter
 - fold
 - unfold
-}
