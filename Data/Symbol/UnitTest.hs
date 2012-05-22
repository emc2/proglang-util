module Data.Symbol.UnitTest(tests) where

import Data.Symbol
import Test.HUnit

tests = TestList [
  "name (symbol i s) = s" ~: "s" ~?= (name (symbol 1 "s")),
  "(symbol 1 s) == (symbol 1 s)" ~:
    assertBool "" ((symbol 1 "s") == (symbol 1 "s")),
  "unused == unused" ~: assertBool "" (unused == unused),
  "(symbol 1 s) != (symbol 2 s)" ~:
    assertBool "" ((symbol 1 "s") /= (symbol 2 "s")),
  "unused != (symbol 2 s)" ~:
    assertBool "" (unused /= (symbol 2 "s")),
  "(symbol 1 s) != unused" ~:
    assertBool "" ((symbol 1 "s") /= unused),
  "(symbol 1 s) < (symbol 2 s)" ~:
    assertBool "" ((symbol 1 "s") < (symbol 2 "s")),
  "(symbol 2 s) > (symbol 1 s)" ~:
    assertBool "" ((symbol 2 "s") > (symbol 1 "s")),
  "unused < (symbol 2 s)" ~:
    assertBool "" (unused < (symbol 2 "s")),
  "(symbol 2 s) > unused" ~:
    assertBool "" ((symbol 2 "s") > unused),
  "number (symbol 1 s) = 1" ~: 1 ~?= (number (symbol 1 "s"))
  ]