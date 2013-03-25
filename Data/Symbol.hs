-- Copyright (c) 2013 Eric McCorkle.  All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
--
-- 1. Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
--
-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
--
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
{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}

-- | This module provides an implementation of a very common technique
-- in compiler implementation.  Names are assigned to a unique number
-- during lexing, which allows them to be compared with a simple
-- numerical equality check thereafter.  This also allows symbols to
-- be stored in arrays as opposed to hash maps.
module Data.Symbol(
       Symbol,
       -- * Global values
       unused,
       firstIndex,
       -- * Accessors
       symbol,
       name,
       number
       ) where

import Data.Default
import Data.Hashable
import Data.Word
--import Test.QuickCheck
import Text.Format

-- | The symbol datatype.  A symbol consists of an index and a name.
-- The index uniquely identifies the symbol.  The name is only present
-- for output.
data Symbol = Symbol !Word !String

-- | A symbol representing an unused name.  This is largely to provide
-- a single value which can be recognized as unused, without having to
-- get it from a gensym.
--
-- This is most useful in the context of a dependent type system.
unused :: Symbol
unused = Symbol 0 ""

-- | Any symbols should be created starting with this index.
firstIndex :: Word
firstIndex = 1

-- | Create a symbol
symbol :: Word
       -- ^ Index value (this determines equality and ordering)
       -> String
       -- ^ Name (this is ignored)
       -> Symbol
       -- ^ Symbol value
symbol = Symbol

-- | Get the name of a symbol
name :: Symbol -> String
name (Symbol _ s) = s

-- | Get the numerical index of the symbol
number :: Symbol -> Word
number (Symbol n _) = n

instance Hashable Symbol where
  hashWithSalt s (Symbol n _) = hashWithSalt s n

instance Format Symbol where
  format (Symbol 0 _) = text "<unused>"
  format (Symbol _ s) = text s

instance Show Symbol where
  show = show . format

instance Ord Symbol where
  compare (Symbol n1 _) (Symbol n2 _) = compare n1 n2

instance Eq Symbol where
  (Symbol n1 _) == (Symbol n2 _) = n1 == n2

instance Default Symbol where
  defaultVal = unused