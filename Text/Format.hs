{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
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

-- | This module contains a common class for pretty printable objects,
-- and some utility code for formatting them.
--
-- Note, this needs to be expanded to allow for some kind of naming
-- function to be passed in to format, in order to allow numerical
-- identifiers to be translated into names.
module Text.Format(
       Format(..),
       FormatList(..),
       Doc,
       -- * Concatenation
       (<>),
       (<+>),
       ($$),
       ($+$),
       hcat,
       hsep,
       vcat,
       cat,
       sep,
       fcat,
       fsep,
       -- * Constants
       empty,
       semi,
       comma,
       colon,
       equals,
       space,
       lparen,
       rparen,
       lbrack,
       rbrack,
       lbrace,
       rbrace,
       -- * Text
       text,
       -- * Wrapping
       parens,
       brackets,
       braces,
       quotes,
       doubleQuotes,
       -- * Nesting
       nest,
       hang,
       punctuate,
       -- * Blocks
       block,
       parenBlock,
       braceBlock,
       bracketBlock,
       -- * Lists
       headlessParenList,
       headlessBraceList,
       headlessBracketList,
       parenList,
       braceList,
       bracketList
       ) where

import Data.Int
import Data.Word
import Text.PrettyPrint(Doc)

import qualified Text.PrettyPrint as PP

-- | An empty document
empty = PP.empty

-- | A Semicolon
semi = PP.semi

-- | A comma
comma = PP.comma

-- | A colon
colon = PP.colon

-- | An equal sign
equals = PP.equals

-- | A space
space = PP.space

-- | A left paren
lparen = PP.lparen

-- | A right paren
rparen = PP.rparen

-- | A left bracket
lbrack = PP.lbrack

-- | A right bracket
rbrack = PP.rbrack

-- | A left brace
lbrace = PP.lbrace

-- | A right brace
rbrace = PP.rbrace

-- | Format text as a document
text = PP.text

-- | Enclose the given document in parentheses
parens :: Format f => f -> Doc
parens = PP.parens . format

-- | Enclose the given document in brackets
brackets :: Format f => f -> Doc
brackets = PP.brackets . format

-- | Enclose the given document in braces
braces :: Format f => f -> Doc
braces = PP.braces . format

-- | Enclose the given document in single quotes
quotes :: Format f => f -> Doc
quotes = PP.quotes . format

-- | Enclose the given document in double quotes
doubleQuotes :: Format f => f -> Doc
doubleQuotes = PP.doubleQuotes . format

-- | Concatenate two documents
(<>) :: (Format f, Format g) => f -> g -> Doc
f <> g = (format f) PP.<> (format g)

-- | Concatenate two documents without space
(<+>) :: (Format f, Format g) => f -> g -> Doc
f <+> g = (format f) PP.<+> (format g)

($$) :: (Format f, Format g) => f -> g -> Doc
f $$ g = (format f) PP.$$ (format g)

($+$) :: (Format f, Format g) => f -> g -> Doc
f $+$ g = (format f) PP.$+$ (format g)

hcat :: Format f => [f] -> Doc
hcat = PP.hcat . map format

hsep :: Format f => [f] -> Doc
hsep = PP.hsep . map format

vcat :: Format f => [f] -> Doc
vcat = PP.vcat . map format

cat :: Format f => [f] -> Doc
cat = PP.cat . map format

sep :: Format f => [f] -> Doc
sep = PP.sep . map format

fcat :: Format f => [f] -> Doc
fcat = PP.fcat . map format

fsep :: Format f => [f] -> Doc
fsep = PP.fsep . map format

-- | Nest the argument some number of indentation layers down.
nest :: Format f => Int
     -- ^ Indentation
     -> f
     -- ^ Content
     -> Doc
nest n = PP.nest n . format

hang :: (Format f, Format g) => f -> Int -> g -> Doc
hang f n = PP.hang (format f) n . format

-- | Add content to the end of every member of a list.  Most often
-- used with comma or semicolon.
punctuate :: (Format f, Format g) => f
          -- ^ Punctuation
          -> [g]
          -- ^ List
          -> [Doc]
punctuate f = PP.punctuate (format f) . map format

block :: (Format f, Format g, Format h) =>
         Int -> f -> g -> h -> Doc
block lvl head body tail =
  sep [ hang head lvl body, format tail ]

parenBlock :: (Format f, Format g) => f -> [g] -> Doc
parenBlock head body =
  block 2 (head <+> lparen) (sep body) rparen

bracketBlock :: (Format f, Format g) => f -> [g] -> Doc
bracketBlock head body =
  block 2 (head <+> lbrack) (sep body) rbrack

braceBlock :: (Format f, Format g) => f -> [g] -> Doc
braceBlock head body =
  block 2 (head <+> lbrace) (sep body) rbrace

headlessParenList :: Format f => [f] -> Doc
headlessParenList body =
  lparen <> (nest 2 (sep (punctuate comma body))) <> rparen

headlessBracketList :: Format f => [f] -> Doc
headlessBracketList body =
  lbrack <+> (nest 2 (sep (punctuate comma body))) <+> rbrack

headlessBraceList :: Format f => [f] -> Doc
headlessBraceList body =
  lbrace <+> (nest 2 (sep (punctuate comma body))) <+> rbrace

parenList :: (Format f, Format g) => f -> [g] -> Doc
parenList head body =
  head <> headlessParenList body

bracketList :: (Format f, Format g) => f -> [g] -> Doc
bracketList head body =
  head <+> headlessBracketList body

braceList :: (Format f, Format g) => f -> [g] -> Doc
braceList head body =
  head <+> headlessBracketList body

-- | A class representing entities that can be formatted
class Format f where
  -- | Format the given entity
  format :: f -> Doc

instance Format Doc where
  format d = d

instance Format String where
  format s = PP.text s

instance Format Bool where
  format i = PP.text (show i)

instance Format Int where
  format i = PP.int i

instance Format Word where
  format i = PP.text (show i)

instance Format Int8 where
  format i = PP.text (show i)

instance Format Int16 where
  format i = PP.text (show i)

instance Format Int32 where
  format i = PP.text (show i)

instance Format Int64 where
  format i = PP.text (show i)

instance Format Word8 where
  format i = PP.text (show i)

instance Format Word16 where
  format i = PP.text (show i)

instance Format Word32 where
  format i = PP.text (show i)

instance Format Word64 where
  format i = PP.text (show i)

instance Format Integer where
  format i = PP.integer i

instance Format Char where
  format c = PP.char c

instance Format Float where
  format f = PP.float f

instance Format Double where
  format d = PP.double d

instance Format Rational where
  format r = PP.rational r

-- | A class representing entities that can be formatted as a list of
-- documents.  Good for bindings, scopes, etc.
class FormatList f where
  formatList :: f -> [Doc]

instance Format f => FormatList [f] where
  formatList f = map format f