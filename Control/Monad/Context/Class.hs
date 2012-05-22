{-# LANGUAGE MultiParamTypeClasses #-}
module Control.Monad.Context.Class(
       MonadContext,
       context
       ) where

class Monad m => MonadContext a m where
  context :: m a
