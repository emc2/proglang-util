module UnitTestMain where

import IO
import Test.HUnit
import UnitTest

main :: IO ()
main =
  do
    (_, shows) <- runTestText putTextToShowS tests
    putStr (shows "")
    return ()
