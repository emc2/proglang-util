{-# LANGUAGE UndecidableInstances, MultiParamTypeClasses,
	     FlexibleInstances #-}
module Control.Monad.Symtab(
       MonadSymtab,
       insert,
       lookup,
       list
       ) where

import Control.Monad.State
import Control.Monad.Symtab.Class
import Prelude((.))

instance MonadSymtab a m => MonadSymtab a (StateT s m) where
  insert s = lift . (insert s)
  lookup = lift . lookup
  list = lift list
