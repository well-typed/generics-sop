module Main (main, toTreeC) where

import Data.Proxy
import Generics.SOP
import HTransExample
import TestInstances

-- Tests
main :: IO ()
main = do
  print tree
  print $ datatypeInfo (Proxy :: Proxy Tree)
  print treeB
  print $ datatypeInfo (Proxy :: Proxy TreeB)
  print treeC
  print treeDatatypeInfo
  print demotedTreeDatatypeInfo
  print (treeDatatypeInfo == demotedTreeDatatypeInfo)
  print $ convertFull tree
