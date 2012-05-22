module Control.UnitTest(tests) where

import Test.HUnit

import qualified Control.Monad.UnitTest as Monad

tests = TestList [
  "Control.Monad" ~: Monad.tests
  ]