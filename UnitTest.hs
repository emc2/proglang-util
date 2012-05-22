module UnitTest(tests) where

import Test.HUnit

import qualified Control.UnitTest as Control
import qualified Data.UnitTest as Data

tests = TestList [
  "Data" ~: Data.tests,
  "Control" ~: Control.tests
  ]
