{-# LANGUAGE MultiParamTypeClasses #-}
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

-- | This module provides two monad classes for implementing lexers:
-- one representing common lexer state, and one representing keyword
-- lookup tables.
module Control.Monad.Lexer.Class(
       MonadLexer(..),
       MonadKeyword(..),
       ) where

import Data.Pos

-- | This class represents a state monad that holds data that is
-- required by typical Alex lexers.
class Monad m => MonadLexer m where
  -- | The remaining input
  input :: m String
  -- | The lookahead character
  lookahead :: m Char
  -- | The start code (ie the lexer state)
  startCode :: m Int
  -- | The current position
  position :: m Pos
  -- | Set the lexer state, usually after lexing a token
  setLexerState :: String
                -- ^ Input
                -> Char
                -- ^ Lookahead
                -> Pos
                -- ^ Position
                -> m ()
  -- | Set the start code (lexer state).  This is separate, as it is
  -- rarer for a lexer to change states.
  setStartCode :: Int -> m ()

-- | This class represents a state monad allowing lexers to quickly
-- lookup keywords.  This can also easily be retrofitted to provide
-- Gensym-like functionality.
class Monad m => MonadKeyword t m where
  -- | Lookup the keyword for this string
  keyword :: String -> m (Maybe t)
  -- | Add a keyword to the lookup table
  addKeyword :: String -> t -> m ()