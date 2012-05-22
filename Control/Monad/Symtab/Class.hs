{-# LANGUAGE UndecidableInstances, MultiParamTypeClasses #-}
module Control.Monad.Symtab.Class(
       MonadSymtab,
       insert,
       lookup,
       list
       ) where

import Data.Symbol
import Prelude(Maybe, Monad)

class Monad m => MonadSymtab a m where
  insert :: Symbol -> a -> m (Maybe a)
  lookup :: Symbol -> m (Maybe a)
  list :: m [(Symbol,a)]
