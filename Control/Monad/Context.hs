{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses,
	     FlexibleInstances, UndecidableInstances #-}
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

-- | This module provides a simple implementation of the Context
-- class.
module Control.Monad.Context(
       MonadContext(..),
       Context(..),
       ContextT(..),
       ) where

import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Context.Class
{--
import Gensym.Class
import Symtab.Flat.Class
--}
import Prelude(Monad, IO, ($), (.))
{--
import qualified TypeContext.Class as TC
import qualified TypeCheckContext.Class as TCC
--}
newtype Context c a = Context { runContext :: c -> IO a }
newtype ContextT c m a = ContextT { runContextT :: c -> m a }

instance Monad (Context c) where
  return a = Context $ \c -> return a
  (Context x) >>= f =
    Context $
      \c ->
        do
          v <- x c
          runContext (f v) c

instance MonadIO (Context c) where
  liftIO m = Context $ \_ -> liftIO m

instance MonadContext c (Context c) where
  context = Context $ \c -> return c

instance MonadContext c (State c) where
  context = get

instance Monad m => Monad (ContextT c m) where
  return a = ContextT $ \c -> return a
  (ContextT x) >>= f =
    ContextT $
      \c ->
        do
          v <- x c
          runContextT (f v) c

instance MonadIO m => MonadIO (ContextT c m) where
  liftIO m = ContextT $ \_ -> liftIO m

instance Monad m => MonadContext c (ContextT c m) where
  context = ContextT $ \c -> return c

instance Monad m => MonadContext c (StateT c m) where
  context = get

instance MonadTrans (ContextT c) where
  lift m = ContextT $ \_ -> m

instance MonadContext c m => MonadContext c (StateT s m) where
  context = lift context

instance MonadState s m => MonadState s (ContextT c m) where
  get = lift get
  put = lift . put
{--
instance MonadGensym m => MonadGensym (ContextT c m) where
  symbol = lift . symbol
  unique = lift . unique

instance MonadFlatSymtab a m =>
         MonadFlatSymtab a (ContextT c m) where
  insert s = lift . (insert s)
  lookup = lift . lookup
  list = lift list

instance TC.MonadTypeContext m =>
         TC.MonadTypeContext (ContextT c m) where
  insert s ty = lift . TC.insert s ty
  lookup = lift . TC.lookup
  unique s = lift . TC.unique s

instance TCC.MonadTypeCheckContext m =>
         TCC.MonadTypeCheckContext (ContextT c m) where
  insert s ty = lift . TCC.insert s ty
  lookup = lift . TCC.lookup
  unique s = lift . TCC.unique s
  msg = lift . TCC.msg
  msgs = lift TCC.msgs
  assertion = lift . TCC.assertion
  assertions = lift TCC.assertions
--}
