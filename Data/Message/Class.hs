{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
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

-- | This module defines a class for compiler error messages.
module Data.Message.Class(
       Message(..),
       Severity(..)       
       ) where

import Data.Pos
import Text.Format

-- | A type indicating the severity of a reported error
data Severity =
  -- | Indicates a bug in the compiler itself
    Internal
  -- | An error (ie syntax error, type error)
  | Error
  -- | A warning
  | Warning
  -- | Information, not indicating a warning
  | Info

-- | A class representing a compiler message
class (Ord m, Format m, Position m) => Message m where
  -- | Get the message's severity
  severity :: m -> Severity
  -- | Format the message's payload, do not include position or
  -- severity.
  describe :: m -> Doc

instance Format Severity where
  format Internal = text "internal error"
  format Error = text "error"
  format Warning = text "warning"
  format Info = text "info"