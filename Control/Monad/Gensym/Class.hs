module Control.Monad.Gensym.Class(
       MonadGensym,
       symbol,
       unique
       ) where

import Data.Symbol(Symbol)

class Monad m => MonadGensym m where
  symbol :: String -> m Symbol
  unique :: String -> m Symbol
