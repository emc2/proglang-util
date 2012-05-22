module Data.UnitTest(tests) where

import Test.HUnit

import qualified Data.Symbol.UnitTest as Symbol

tests = TestList [
  "Symbol" ~: Symbol.tests
  ]