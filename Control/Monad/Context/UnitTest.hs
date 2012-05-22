module Control.Monad.Context.UnitTest(tests) where

import Control.Monad.Context
import Control.Monad.Trans
import Test.HUnit

run :: a -> IO a
run c = runContext (do v <- context
                       return v)
                   c

tests = TestList [
  "Extract context" ~: do
                         val <- run 4
                         val @?= 4
  ]