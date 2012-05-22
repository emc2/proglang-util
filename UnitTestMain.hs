module Main where

import IO
import System.Exit(exitFailure)
import Test.HUnit
import UnitTest

main :: IO ()
main =
  do
    (counts, shows) <- runTestText putTextToShowS tests
    putStr (shows "")
    if errors counts /= 0 || failures counts /= 0
      then exitFailure
      else return ()
