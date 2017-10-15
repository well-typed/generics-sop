module Main where

import Data.Monoid
import Generics.SOP
import GHC.Proof

cpureTuple :: Proof
cpureTuple =
  to (SOP (Z (hcpure (Proxy :: Proxy Monoid) (I mempty))))
  ===
  ((Sum 0, Product 1, []) :: (Sum Int, Product Int, [Bool]))

cmapTuple :: Proof
cmapTuple =
  concat (hcollapse (hcmap (Proxy :: Proxy Show) (mapIK show) (from ((0 :: Int), False, 'x'))))
  ===
  concat [show (0 :: Int), show False, show 'x']

cmapTuple' :: Proof
cmapTuple' =
  (concat . hcollapse . hcmap (Proxy :: Proxy Show) (mapIK show) . from)
  ===
  ((\ (x, y, z) -> concat [show x, show y, show z]) :: (Int, Bool, Char) -> String)

main :: IO ()
main = return ()
