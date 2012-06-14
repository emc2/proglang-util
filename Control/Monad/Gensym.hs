{-# LANGUAGE UndecidableInstances, FlexibleInstances,
	     MultiParamTypeClasses #-}
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

-- | This module provides implementations of a Gensym monad, as well
-- as a monad transformer.
module Control.Monad.Gensym(
       MonadGensym(..),
       Gensym,
       GensymT,
       runGensym,
       runGensymT,
       ) where

import Control.Monad.Reader
import Control.Monad.Lexer
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Gensym.Class
import Control.Monad.Symtab.Class
import Data.Word
import Prelude hiding (lookup)

import qualified Data.Symbol as Symbol
import qualified Data.HashTable as HashTable

newtype GensymT m a =
  GensymT ((StateT Word (ReaderT (HashTable.HashTable String Symbol.Symbol) m)) a)
type Gensym a = GensymT IO a

unpackGensymT (GensymT g) = g

initGensym :: MonadIO m => m (HashTable.HashTable String Symbol.Symbol)
initGensym =
    do
      tab <- liftIO (HashTable.new (==) HashTable.hashString)
      return tab

-- | Execute the computation represented by a gensym monad
runGensym :: Gensym a -> IO a
runGensym g = runGensymT g

-- | Execute the computation wrapped in a gensym monad transformer
runGensymT :: MonadIO m => GensymT m a -> m a
runGensymT (GensymT g) =
  do
    tab <- initGensym
    (res, _) <- runReaderT (runStateT g Symbol.firstIndex) tab
    return res

symbol' :: MonadIO m => String ->
           (StateT Word (ReaderT (HashTable.HashTable String Symbol.Symbol) m)) Symbol.Symbol
symbol' name =
  do
    tab <- ask
    result <- liftIO (HashTable.lookup tab name)
    case result of
      Just sym -> return sym
      Nothing ->
        do
          id <- get
          sym <- return (Symbol.symbol id name)
          put (id + 1)
          liftIO (HashTable.insert tab name sym)
          return sym

unique' :: MonadIO m => String ->
           (StateT Word (ReaderT (HashTable.HashTable String Symbol.Symbol) m)) Symbol.Symbol
unique' name =
  do
    id <- get
    put (id + 1)
    return (Symbol.symbol id name)

instance Monad m => Monad (GensymT m) where
  return = GensymT . return
  (GensymT x) >>= f = GensymT $ x >>= unpackGensymT . f

instance MonadIO m => MonadGensym (GensymT m) where
  symbol = GensymT . symbol'
  unique = GensymT . unique'

instance MonadIO m => MonadIO (GensymT m) where
  liftIO = GensymT . liftIO

instance MonadTrans GensymT where
  lift = GensymT . lift . lift

instance MonadGensym m => MonadGensym (StateT s m) where
  symbol = lift . symbol
  unique = lift . unique

instance MonadState s m => MonadState s (GensymT m) where
  get = lift get
  put = lift . put

instance MonadLexer m => MonadLexer (GensymT m) where
  input = lift input
  lookahead = lift lookahead
  startCode = lift startCode
  position = lift position
  setLexerState input lookahead = lift . setLexerState input lookahead
  setStartCode code = lift (setStartCode code)

instance MonadReader r m => MonadReader r (GensymT m) where
  ask = lift ask
  local f = GensymT . mapStateT (mapReaderT (local f)) . unpackGensymT

instance MonadKeyword t m => MonadKeyword t (GensymT m) where
  keyword = lift . keyword
  addKeyword str = lift . addKeyword str

instance MonadSymtab a m => MonadSymtab a (GensymT m) where
  insert s = lift . (insert s)
  lookup = lift . lookup
  list = lift list
