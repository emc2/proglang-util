{-# LANGUAGE MultiParamTypeClasses, UndecidableInstances,
             FlexibleInstances #-}
-- Copyright (c) 2012 Eric McCorkle.  All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
-- 1. Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
-- 3. Neither the name of the author nor the names of any contributors
--    may be used to endorse or promote products derived from this software
--    without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE AUTHORS AND CONTRIBUTORS ``AS IS''
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
-- TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
-- PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS
-- OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
-- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
-- LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
-- USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
-- ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
-- OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
-- OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
-- SUCH DAMAGE.

-- | This module provides a monad implementing the Symtab class, as
-- well as an equivalent monad tranformer.
--
-- This module is named "Flat" to distinguish it from scoped symbol
-- tables.  However, it is questionable whether even the most
-- efficient implementation of scoped symbol tables is better than a
-- purely-functional map, and an implementation thereof would likely
-- require FFI calls.  Hence, it remains unimplemented.
module Control.Monad.Symtab.Flat(
       MonadSymtab(..),
       FlatSymtabT,
       runFlatSymtab,
       runFlatSymtabT
       ) where

import Control.Monad.Reader
import Control.Monad.Gensym.Class
import Control.Monad.State
import Control.Monad.Symtab
import Data.Hash
import Data.Int
import Data.Symbol(Symbol)
import Prelude(Maybe(..), IO, String, (==), (.), ($), fromInteger, toInteger)

import qualified Data.HashTable as HashTable

newtype FlatSymtabT a m b =
  FlatSymtabT ((ReaderT (HashTable.HashTable Symbol a) m) b)
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
    runReaderT fs tab

instance MonadIO m => MonadSymtab a (FlatSymtabT a m) where
  insert sym d =
    FlatSymtabT $
      do
        tab <- ask
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
        tab <- ask
        liftIO (HashTable.lookup tab sym)

  list =
    FlatSymtabT $
      do
        tab <- ask
        liftIO (HashTable.toList tab)

instance Monad m => Monad (FlatSymtabT a m) where
  return = FlatSymtabT . return
  (FlatSymtabT x) >>= f = FlatSymtabT $ x >>= unpackFlatSymtabT . f

instance MonadIO m => MonadIO (FlatSymtabT a m) where
  liftIO = FlatSymtabT . liftIO

instance MonadTrans (FlatSymtabT a) where
  lift = FlatSymtabT . lift

instance MonadReader c m => MonadReader c (FlatSymtabT a m) where
  ask = lift ask
  local f = FlatSymtabT . mapReaderT (local f) . unpackFlatSymtabT

instance MonadState s m => MonadState s (FlatSymtabT a m) where
  get = lift get
  put = lift . put

instance MonadGensym m => MonadGensym (FlatSymtabT a m) where
  symbol = lift . symbol
  unique = lift . unique
