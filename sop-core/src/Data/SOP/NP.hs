-- | n-ary products (and products of products)
module Data.SOP.NP
  ( -- * Datatypes
    NP(Nil, (:*))
  , POP(..)
  , unPOP
    -- * Constructing products
  , pure_NP
  , pure_POP
  , cpure_NP
  , cpure_POP
    -- ** Construction from a list
  , fromList
    -- * Application
  , ap_NP
  , ap_POP
    -- * Destructing products
  , hd
  , tl
  , Projection
  , projections
  , shiftProjection
    -- * Lifting / mapping
  , liftA_NP
  , liftA_POP
  , liftA2_NP
  , liftA2_POP
  , liftA3_NP
  , liftA3_POP
  , map_NP
  , map_POP
  , zipWith_NP
  , zipWith_POP
  , zipWith3_NP
  , zipWith3_POP
  , cliftA_NP
  , cliftA_POP
  , cliftA2_NP
  , cliftA2_POP
  , cliftA3_NP
  , cliftA3_POP
  , cmap_NP
  , cmap_POP
  , czipWith_NP
  , czipWith_POP
  , czipWith3_NP
  , czipWith3_POP
    -- * Dealing with @'All' c@
  , hcliftA'
  , hcliftA2'
  , hcliftA3'
  , cliftA2'_NP
    -- * Collapsing
  , collapse_NP
  , collapse_POP
    -- * Folding and sequencing
  , ctraverse__NP
  , ctraverse__POP
  , traverse__NP
  , traverse__POP
  , cfoldMap_NP
  , cfoldMap_POP
  , sequence'_NP
  , sequence'_POP
  , sequence_NP
  , sequence_POP
  , ctraverse'_NP
  , ctraverse'_POP
  , traverse'_NP
  , traverse'_POP
  , ctraverse_NP
  , ctraverse_POP
    -- * Catamorphism and anamorphism
  , cata_NP
  , ccata_NP
  , ana_NP
  , cana_NP
    -- * Transformation of index lists and coercions
  , trans_NP
  , trans_POP
  , coerce_NP
  , coerce_POP
  , fromI_NP
  , fromI_POP
  , toI_NP
  , toI_POP
  ) where

import Data.SOP.NP.Internal
