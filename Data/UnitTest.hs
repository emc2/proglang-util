module Data.UnitTest(tests) where

import Test.HUnit

import qualified Data.Symbol.UnitTest as Symbol
import qualified Data.RingMonomial.UnitTest as RingMonomial
import qualified Data.FieldMonomial.UnitTest as FieldMonomial
import qualified Data.RingPolynomial.UnitTest as RingPolynomial

tests = TestList [
  "Symbol" ~: Symbol.tests,
  "RingMonomial" ~: RingMonomial.tests,
  "FieldMonomial" ~: FieldMonomial.tests,
  "RingPolynomial" ~: RingPolynomial.tests
  ]