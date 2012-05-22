module Control.Monad.Gensym.UnitTest(tests) where

import Control.Monad.Gensym
import Control.Monad.Trans
import Data.Symbol(Symbol)
import Test.HUnit

symssyms :: IO (Symbol, Symbol)
symssyms = runGensym
  (do
    s1 <- symbol "s"
    s2 <- symbol "s"
    return (s1, s2))

symssymt :: IO (Symbol, Symbol)
symssymt = runGensym
  (do
    s <- symbol "s"
    t <- symbol "t"
    return (s, t))

symsunis :: IO (Symbol, Symbol)
symsunis = runGensym
  (do
    s1 <- symbol "s"
    s2 <- unique "s"
    return (s1, s2))

unissyms :: IO (Symbol, Symbol)
unissyms = runGensym
  (do
    s1 <- unique "s"
    s2 <- symbol "s"
    return (s1, s2))

unisunis :: IO (Symbol, Symbol)
unisunis = runGensym
  (do
    s1 <- unique "s"
    s2 <- unique "s"
    return (s1, s2))

symsunissyms :: IO (Symbol, Symbol, Symbol)
symsunissyms = runGensym
  (do
    s1 <- symbol "s"
    s2 <- unique "s"
    s3 <- symbol "s"
    return (s1, s2, s3))

symsunisunis :: IO (Symbol, Symbol, Symbol)
symsunisunis = runGensym
  (do
    s1 <- symbol "s"
    s2 <- unique "s"
    s3 <- unique "s"
    return (s1, s2, s3))

unissymssyms :: IO (Symbol, Symbol, Symbol)
unissymssyms = runGensym
  (do
    s1 <- unique "s"
    s2 <- symbol "s"
    s3 <- symbol "s"    
    return (s1, s2, s3))

unissymsunis :: IO (Symbol, Symbol, Symbol)
unissymsunis = runGensym
  (do
    s1 <- unique "s"
    s2 <- symbol "s"
    s3 <- unique "s"    
    return (s1, s2, s3))

tests = TestList [
  "symbol s; symbol s" ~: do (s1, s2) <- symssyms
                             assertBool (show s1 ++ " == " ++ show s2)
                                        (s1 == s2),
  "symbol s; symbol t" ~: do (s1, s2) <- symssymt
                             assertBool (show s1 ++ " /= " ++ show s2)
                                        (s1 /= s2),
  "symbol s; unique s" ~: do (s1, s2) <- symsunis
                             assertBool (show s1 ++ " /= " ++ show s2)
                                        (s1 /= s2),
  "unique s; symbol s" ~: do (s1, s2) <- unissyms
                             assertBool (show s1 ++ " /= " ++ show s2)
                                        (s1 /= s2),
  "unique s; unique s" ~: do (s1, s2) <- unisunis
                             assertBool (show s1 ++ " /= " ++ show s2)
                                        (s1 /= s2),
  "symbol s; unique s; symbol s" ~:
    do (s1, s2, s3) <- symsunissyms
       assertBool (show s1 ++ " /= " ++ show s2) (s1 /= s2)
       assertBool (show s3 ++ " /= " ++ show s2) (s3 /= s2)
       assertBool (show s1 ++ " == " ++ show s3) (s1 == s3),
  "symbol s; unique s; unique s" ~:
    do (s1, s2, s3) <- symsunisunis
       assertBool (show s1 ++ " /= " ++ show s2) (s1 /= s2)
       assertBool (show s3 ++ " /= " ++ show s2) (s3 /= s2)
       assertBool (show s1 ++ " == " ++ show s3) (s1 /= s3),
  "unique s; symbol s; symbol s" ~:
    do (s1, s2, s3) <- unissymssyms
       assertBool (show s1 ++ " /= " ++ show s2) (s1 /= s2)
       assertBool (show s3 ++ " == " ++ show s2) (s3 == s2)
       assertBool (show s1 ++ " /= " ++ show s3) (s1 /= s3),
  "unique s; symbol s; unique s" ~:
     do (s1, s2, s3) <- unissymsunis
        assertBool (show s1 ++ " /= " ++ show s2) (s1 /= s2)
        assertBool (show s3 ++ " /= " ++ show s2) (s3 /= s2)
        assertBool (show s1 ++ " /= " ++ show s3) (s1 /= s3)

  ]