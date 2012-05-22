{-# OPTIONS_GHC -funbox-strict-fields #-}
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

-- | This module provides a monad that implements both MonadLexer and
-- MonadKeyword, as well as a similar monad transformer.
module Control.Monad.Lexer(
       MonadLexer(..),
       MonadKeyword(..),
       Lexer,
       LexerT,
       runLexerT,
       runLexer,
       ) where

import Control.Monad.Lexer.Class
import Control.Monad.State(StateT, runStateT, get, put)
import Control.Monad.Trans
import Data.Pos
import Data.Word

import Data.HashTable(HashTable)
import qualified Data.HashTable as HashTable

data LexerState t =
  LexerState { psInput :: String, psStartCode :: !Int,
               psKeywordTab :: HashTable String t,
               psPos :: !Pos }

newtype LexerT t m a = LexerT ((StateT (LexerState t) m) a)
type Lexer t a = LexerT t IO a

unpackLexerT (LexerT pm) = pm

runLexerT :: MonadIO m => [(String, t)]
          -- ^ Initial keywords
          -> String
          -- ^ Input filename
          -> String
          -- ^ Input string
          -> LexerT t m a
          -- ^ Monad to run
          -> m a
runLexerT keywords filename input (LexerT pm) =
  do
    tab <- liftIO (HashTable.fromList HashTable.hashString keywords)
    (res, _) <- runStateT pm (LexerState { psInput = '\n' : input,
                                           psStartCode = 0, psKeywordTab = tab,
                                           psPos = point filename 1 1 0 })
    return res

runLexer :: [(String, t)]
         -- ^ Initial keywords
         -> String
         -- ^ Input filename
         -> String
         -- ^ Input string
         -> Lexer t a
         -- ^ Monad to run
         -> IO a
runLexer keywords filename input m = runLexerT keywords filename input m

input' :: Monad m => (StateT (LexerState t) m) String
input' =
  do
    st <- get
    return (tail (psInput st))

lookahead' :: Monad m => (StateT (LexerState t) m) Char
lookahead' =
  do
    st <- get
    return (head (psInput st))

startCode' :: Monad m => (StateT (LexerState t) m) Int
startCode' =
  do
    st <- get
    return (psStartCode st)

position' :: Monad m => (StateT (LexerState t) m) Pos
position' =
  do
    st <- get
    return (psPos st)

setLexerState' :: Monad m =>
                  String -> Char -> Pos -> (StateT (LexerState t) m) ()
setLexerState' input lookahead pos =
  do
    st <- get
    put (st { psPos = pos, psInput = (lookahead :input) })

setStartCode' :: Monad m => Int -> (StateT (LexerState t) m) ()
setStartCode' startCode =
  do
    st <- get
    put (st { psStartCode = startCode })

keyword' :: MonadIO m => String -> (StateT (LexerState t) m) (Maybe t)
keyword' str =
  do
    st <- get
    liftIO (HashTable.lookup (psKeywordTab st) str)

addKeyword' :: MonadIO m => String -> t -> (StateT (LexerState t) m) ()
addKeyword' str tok =
  do
    st <- get
    liftIO (HashTable.insert (psKeywordTab st) str tok)

instance Monad m => Monad (LexerT t m) where
  return = LexerT . return
  (LexerT x) >>= f =
    LexerT $ x >>= unpackLexerT . f

instance MonadIO m => MonadLexer (LexerT t m) where
  input = LexerT input'
  lookahead = LexerT lookahead'
  startCode = LexerT startCode'
  position = LexerT position'
  setLexerState input lookahead = LexerT . setLexerState' input lookahead
  setStartCode = LexerT . setStartCode'

instance MonadIO m => MonadKeyword t (LexerT t m) where
  keyword = LexerT . keyword'
  addKeyword str = LexerT . addKeyword' str

instance MonadIO m => MonadIO (LexerT t m) where
  liftIO = LexerT . liftIO

instance MonadTrans (LexerT t) where
  lift = LexerT . lift