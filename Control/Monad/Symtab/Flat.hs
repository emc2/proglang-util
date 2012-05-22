{-# OPTIONS_GHC -fglasgow-exts #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Monad.Symtab.Flat(
       MonadSymtab,
       FlatSymtabT,
       lookup,
       insert,
       list,
       runFlatSymtab,
       runFlatSymtabT
       ) where

import Control.Monad.Context
import Control.Monad.Gensym.Class
import Control.Monad.State
import Control.Monad.Symtab
import Data.Hash
import Data.Int
import Data.Symbol(Symbol)
import Prelude(Maybe(..), IO, String, (==), (.), ($), fromInteger, toInteger)

import qualified Data.HashTable as HashTable

newtype FlatSymtabT a m b =
  FlatSymtabT ((ContextT (HashTable.HashTable Symbol a) m) b)
type FlatSymtab a b = FlatSymtabT a IO b

unpackFlatSymtabT (FlatSymtabT t) = t

hashSym :: Symbol -> Int32
hashSym = fromInteger . toInteger . asWord64 . hash

initFlatSymtab :: MonadIO m => m (HashTable.HashTable Symbol a)
initFlatSymtab = liftIO (HashTable.new (==) hashSym)

runFlatSymtab :: FlatSymtab a b -> IO b
runFlatSymtab fs = runFlatSymtabT fs

runFlatSymtabT :: MonadIO m => FlatSymtabT a m b -> m b
runFlatSymtabT (FlatSymtabT fs) =
  do
    tab <- initFlatSymtab
    runContextT fs tab

instance MonadIO m => MonadSymtab a (FlatSymtabT a m) where
  insert sym d =
    FlatSymtabT $
      do
        tab <- context
        old <- liftIO (HashTable.lookup tab sym)
        case old of
          Just _ ->
            do
              liftIO (HashTable.update tab sym d)
              return ()
          Nothing -> liftIO (HashTable.insert tab sym d)
        return old

  lookup sym =
    FlatSymtabT $
      do
        tab <- context
        liftIO (HashTable.lookup tab sym)

  list =
    FlatSymtabT $
      do
        tab <- context
        liftIO (HashTable.toList tab)

instance Monad m => Monad (FlatSymtabT a m) where
  return = FlatSymtabT . return
  (FlatSymtabT x) >>= f = FlatSymtabT $ x >>= unpackFlatSymtabT . f

instance MonadIO m => MonadIO (FlatSymtabT a m) where
  liftIO = FlatSymtabT . liftIO

instance MonadTrans (FlatSymtabT a) where
  lift = FlatSymtabT . lift

instance MonadContext c m => MonadContext c (FlatSymtabT a m) where
  context = lift context

instance MonadState s m => MonadState s (FlatSymtabT a m) where
  get = lift get
  put = lift . put

instance MonadGensym m => MonadGensym (FlatSymtabT a m) where
  symbol = lift . symbol
  unique = lift . unique
