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

-- | This module contains various bits of functionality that are
-- useful in dealing with the Alex lexer generator.
module Text.AlexHelper(
       -- * Comment stripping
       -- | Some languages allow operators to be defined by the
       -- language.  Getting the token rules right in order for a
       -- comment start to not get lexed as part of or all of an
       -- operator can be gnarly.  It's often just easier to strip the
       -- comments as a preprocessing stage.
       --
       -- These also handle nested comments properly (including
       -- stripCComment; non-nestable C comments are easy to implement
       -- with lexer rules).

       stripLine,
       stripCComment,
       stripSMLComment,

       -- * Parse strings to numbers
       -- | The apparent absence of these functions is a notable
       -- deficiency in Haskell's standard library

       parseBinNum,
       parseOctNum,
       parseHexNum,

       -- * Miscellaneous
       andBegin
       ) where

import Control.Monad.Lexer.Class
import Data.Pos

-- | Strip the remainder of the line from input
stripLine :: Pos
          -- ^ Starting position
          -> String
          -- ^ Input string
          -> (Pos, String)
          -- ^ New starting position and input string
stripLine p "" = (p, "")
stripLine p ('\n':rest) = (newline p, rest)
stripLine p (_:rest) = stripLine (advance 1 p) rest

-- | Strip a nestable C-style comment
stripCComment :: Pos
              -- ^ Starting position
              -> String
              -- ^ Input string
              -> (String, Bool, Pos)
              -- ^ Input string, whether the comment was terminated,
              -- new starting position
stripCComment pos "" = (undefined, False, pos)
stripCComment pos ('/':'*':rest) =
  case stripCComment (advance 2 pos) rest of
    (_, False, pos) -> ("", False, pos)
    (rest, True, pos) -> stripCComment pos rest
stripCComment pos ('*':'/':rest) = (rest, True, advance 2 pos)
stripCComment pos ('\n':rest) = stripCComment (newline pos) rest
stripCComment pos (_:rest) = stripCComment (advance 1 pos) rest

-- | Strip a nestable SML-style comment
stripSMLComment :: Pos
                -- ^ Starting position
                -> String
                -- ^ Input string
                -> (String, Bool, Pos)
                -- ^ Input string, whether the comment was terminated,
                -- new starting position
stripSMLComment pos "" = (undefined, False, pos)
stripSMLComment pos ('(':'*':rest) =
  case stripSMLComment (advance 2 pos) rest of
    (_, False, pos) -> ("", False, pos)
    (rest, True, pos) -> stripSMLComment pos rest
stripSMLComment pos ('*':')':rest) = (rest, True, advance 2 pos)
stripSMLComment pos ('\n':rest) = stripSMLComment (newline pos) rest
stripSMLComment pos (_:rest) = stripSMLComment (advance 1 pos) rest

-- | Parse a string representing a number in binary
parseBinNum :: Num n => String -> n
parseBinNum ('0':'b':rest) =
  let
    parseBin' "" n = n
    parseBin' ('0':rest) n = parseBin' rest (2 * n)
    parseBin' ('1':rest) n = parseBin' rest ((2 * n) + 1)
  in
    parseBin' rest 0

-- | Parse a string representing a number in octal
parseOctNum :: Num n => String -> n
parseOctNum ('0':rest) =
  let
    parseOct' "" n = n
    parseOct' ('0':rest) n = parseOct' rest (8 * n)
    parseOct' ('1':rest) n = parseOct' rest ((8 * n) + 1)
    parseOct' ('2':rest) n = parseOct' rest ((8 * n) + 2)
    parseOct' ('3':rest) n = parseOct' rest ((8 * n) + 3)
    parseOct' ('4':rest) n = parseOct' rest ((8 * n) + 4)
    parseOct' ('5':rest) n = parseOct' rest ((8 * n) + 5)
    parseOct' ('6':rest) n = parseOct' rest ((8 * n) + 6)
    parseOct' ('7':rest) n = parseOct' rest ((8 * n) + 7)
  in
    parseOct' rest 0

-- | Parse a string representing a number in hexadecimal
parseHexNum :: Num n => String -> n
parseHexNum ('0':'x':rest) =
  let
    parseHex' "" n = n
    parseHex' ('0':rest) n = parseHex' rest (0x10 * n)
    parseHex' ('1':rest) n = parseHex' rest ((0x10 * n) + 0x1)
    parseHex' ('2':rest) n = parseHex' rest ((0x10 * n) + 0x2)
    parseHex' ('3':rest) n = parseHex' rest ((0x10 * n) + 0x3)
    parseHex' ('4':rest) n = parseHex' rest ((0x10 * n) + 0x4)
    parseHex' ('5':rest) n = parseHex' rest ((0x10 * n) + 0x5)
    parseHex' ('6':rest) n = parseHex' rest ((0x10 * n) + 0x6)
    parseHex' ('7':rest) n = parseHex' rest ((0x10 * n) + 0x7)
    parseHex' ('8':rest) n = parseHex' rest ((0x10 * n) + 0x8)
    parseHex' ('9':rest) n = parseHex' rest ((0x10 * n) + 0x9)
    parseHex' ('a':rest) n = parseHex' rest ((0x10 * n) + 0xa)
    parseHex' ('b':rest) n = parseHex' rest ((0x10 * n) + 0xb)
    parseHex' ('c':rest) n = parseHex' rest ((0x10 * n) + 0xc)
    parseHex' ('d':rest) n = parseHex' rest ((0x10 * n) + 0xd)
    parseHex' ('e':rest) n = parseHex' rest ((0x10 * n) + 0xe)
    parseHex' ('f':rest) n = parseHex' rest ((0x10 * n) + 0xf)
  in
    parseHex' rest 0

-- | This is used as a "glue" operator to enter a state and begin an
-- action.  It is most often called in the form action `andBegin`
-- code, in order to produce a new action.0;10;0c
andBegin :: MonadLexer m => (String -> Pos -> m a)
         -- ^ The action to perform
         -> Int
         -- ^ The state to enter
         -> String
         -- ^ Input string
         -> Pos
         -- ^ Starting position
         -> m a
andBegin action code input pos =
  do
    setStartCode code
    action input pos
