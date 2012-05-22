module Control.Monad.UnitTest(tests) where

import Test.HUnit

import qualified Control.Monad.Gensym.UnitTest as Gensym
import qualified Control.Monad.Symtab.UnitTest as Symtab

tests = TestList [
  "Control.Monad.Gensym" ~: Gensym.tests,
  "Control.Monad.Symtab" ~: Symtab.tests
  ]