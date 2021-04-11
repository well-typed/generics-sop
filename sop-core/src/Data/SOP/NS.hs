{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}

-- | n-ary sums (and sums of products)
module Data.SOP.NS
  ( -- * Datatypes
    NS(Z, S)
  , SOP(..)
  , unSOP
    -- * Constructing sums
  , Injection
  , injections
  , shift
  , shiftInjection
  , apInjs_NP
  , apInjs'_NP
  , apInjs_POP
  , apInjs'_POP
  -- See also 'emptySOP'.
  --
    -- * Destructing sums
  , emptyNS
  , emptySOP
  , unZ
  , index_NS
  , index_SOP
  , Ejection
  , ejections
  , shiftEjection
    -- * Application
  , ap_NS
  , ap_SOP
    -- * Lifting / mapping
  , liftA_NS
  , liftA_SOP
  , liftA2_NS
  , liftA2_SOP
  , cliftA_NS
  , cliftA_SOP
  , cliftA2_NS
  , cliftA2_SOP
  , map_NS
  , map_SOP
  , cmap_NS
  , cmap_SOP
    -- * Dealing with @'All' c@
  , cliftA2'_NS
    -- * Comparison
  , compare_NS
  , ccompare_NS
  , compare_SOP
  , ccompare_SOP
    -- * Collapsing
  , collapse_NS
  , collapse_SOP
    -- * Folding and sequencing
  , ctraverse__NS
  , ctraverse__SOP
  , traverse__NS
  , traverse__SOP
  , cfoldMap_NS
  , cfoldMap_SOP
  , sequence'_NS
  , sequence'_SOP
  , sequence_NS
  , sequence_SOP
  , ctraverse'_NS
  , ctraverse'_SOP
  , traverse'_NS
  , traverse'_SOP
  , ctraverse_NS
  , ctraverse_SOP
    -- * Catamorphism and anamorphism
  , cata_NS
  , ccata_NS
  , ana_NS
  , cana_NS
    -- * Expanding sums to products
  , expand_NS
  , cexpand_NS
  , expand_SOP
  , cexpand_SOP
    -- * Transformation of index lists and coercions
  , trans_NS
  , trans_SOP
  , coerce_NS
  , coerce_SOP
  , fromI_NS
  , fromI_SOP
  , toI_NS
  , toI_SOP
  ) where

import Data.SOP.NS.Internal
