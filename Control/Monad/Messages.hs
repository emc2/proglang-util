{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances,
             UndecidableInstances #-}
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

module Control.Monad.Messages(
       MonadMessages(..),
       MessagesT,
       Messages,
       runMessagesT,
       runMessages
       ) where

import Control.Monad.Messages.Class
import Control.Monad.Gensym
import Control.Monad.Symtab
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Writer
import Data.List
import Data.Message.Class
import Text.Format

newtype MessagesT w m a = MessagesT ((WriterT [w] m) a)
type Messages w a = MessagesT w IO a

unpackMessagesT (MessagesT w) = w

runMessagesT :: (MonadIO m, Message w) => MessagesT w m a -> m a
runMessagesT (MessagesT m) =
  do
    (res, msgs) <- runWriterT m
    liftIO (mapM_ (putStr . show . format) (sort msgs))
    return res

runMessages :: Message w => Messages w a -> IO a
runMessages m = runMessagesT m

msg' :: Monad m => w -> (WriterT [w] m) ()
msg' m = tell [m]

msgs' :: Monad m => [w] -> (WriterT [w] m) ()
msgs' ms = tell ms

instance Monad m => Monad (MessagesT w m) where
  return = MessagesT . return
  (MessagesT m) >>= f = MessagesT $ m >>= unpackMessagesT . f

instance (Monad m, Message w) => MonadMessages w (MessagesT w m) where
  msg = MessagesT . msg'
  msgs = MessagesT . msgs'

instance MonadIO m => MonadIO (MessagesT w m) where
  liftIO = MessagesT . liftIO

instance MonadTrans (MessagesT w) where
  lift = MessagesT . lift

instance MonadMessages w m => MonadMessages w (StateT s m) where
  msg = lift . msg
  msgs = lift . msgs

instance MonadState s m => MonadState s (MessagesT w m) where
  get = lift get
  put = lift . put
