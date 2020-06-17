{-# LANGUAGE TemplateHaskell #-}
module TestUse where

import Test

testUse :: (Int, Int)
testUse = $$(test [|| Foo 1 ||] [|| Foo 2 ||])

