module SOPBench.Roundtrip where

import qualified Generics.SOP as SOP
import qualified GHC.Generics as GHC

class Roundtrip a where
  roundtrip :: a -> a

soproundtrip :: SOP.Generic a => a -> a
soproundtrip = SOP.to . SOP.from

ghcroundtrip :: GHC.Generic a => a -> a
ghcroundtrip = GHC.to . GHC.from

