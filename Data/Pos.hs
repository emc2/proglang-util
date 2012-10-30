-- Copyright (c) 2012 Eric McCorkle.  All rights reserved.
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
{-# OPTIONS_GHC -funbox-strict-fields #-}

-- | This module contains utilities for representing positions in
-- source files.  This is very commonly used in anything dealing with
-- programming languages.  It also defines a Position class, which
-- represents entities that can be connected with a specific position
-- in source.
module Data.Pos(
       -- * Pos type
       Pos,
       -- * Position class
       Position(..),
       -- * Accessors
       filename,
       startline,
       startcol,
       startoff,
       endline,
       endcol,
       endoff,
       -- | The following accessors are for treating a position as if
       -- it occurs at a single point.  They return the respective
       -- starting position elements.
       line,
       col,
       offset,
       -- * Creation
       range,
       point,
       internal,
       -- * Derivation
       advance,
       newline,
       merge,
       split,
       endpoint
       ) where

import Data.Foldable
import Data.Hash
import Text.Format

-- | This type represents a position in a source code file
data Pos =
  Pos {
    -- | The name of the source file
    filename :: !String,
    -- | The line at which the position starts
    startline :: !Int,
    -- | The column at which the position starts
    startcol :: !Int,
    -- | The absolute offset at which the position starts
    startoff :: !Int,
    -- | The line at which the position ends
    endline :: !Int,
    -- | The column at which the position ends
    endcol :: !Int,
    -- | The offset at which the position ends
    endoff :: !Int }
  deriving Eq

-- | This class denotes anything which originates from a certain
-- position in a source code file.
class Position a where
  -- | Get the position from which this object originates
  pos :: a -> Pos

line :: Pos -> Int
line = startline

col :: Pos -> Int
col = startcol

offset :: Pos -> Int
offset = startoff

-- | Construct a position from a range in a source file
range :: String
      -- ^ The filename
      -> Int
      -- ^ The starting line
      -> Int
      -- ^ The starting column
      -> Int
      -- ^ The starting offset
      -> Int
      -- ^ The ending line
      -> Int
      -- ^ The ending column
      -> Int
      -- ^ The ending offset
      -> Pos
      -- ^ The position
range fname sline scol soff eline ecol eoff =
  Pos { filename = fname, startline = sline, startcol = scol, startoff = soff,
        endline = eline, endcol = ecol, endoff = eoff }

-- | Construct a position occurring at a single point in a source file
point :: String
      -- ^ The filename
      -> Int
      -- ^ The line
      -> Int
      -- ^ The column
      -> Int
      -- ^ The offset
      -> Pos
      -- ^ The position
point fname line col off =
  Pos { filename = fname, startline = line, startcol = col, startoff = off,
        endline = line, endcol = col, endoff = off }

-- | Advance a position by some number of characters.  This will
-- expand the "range" covered by the position.
advance :: Int
        -- ^ Number of characters
        -> Pos
        -- ^ Starting position
        -> Pos
        -- ^ New position
advance off pos =
  let
    ecol = (endcol pos)
    eoff = (endoff pos)
  in
    pos { endcol = ecol + off, endoff = eoff + off }

-- | Advance a position to the next line.  THis will expand the
-- "range" covered by the position.
newline :: Pos -> Pos
newline pos =
  let
    eline = (endline pos)
    eoff = (endoff pos)
  in
    pos { endline = eline + 1, endcol = 1, endoff = eoff + 1 }

-- | Combine two positions to create a new position ranging from the
-- start of the first to the end of the second.
merge :: Pos -> Pos -> Pos
merge start end =
  range (filename start) (startline start) (startcol start) (startoff start)
        (endline end) (endcol end) (endoff end)

-- | Create a position representing the endpoint of the argument.
endpoint :: Pos -> Pos
endpoint pos =
  let
    eline = (endline pos)
    ecol = (endcol pos)
    eoff = (endoff pos)
  in
    pos { startline = eline, startcol = ecol, startoff = eoff }

-- | Split a position into its endpoint, and everything that came
-- before.  Assumes the argument represents a range, not a point.
split :: Pos -> (Pos, Pos)
split pos =
  let
    eline = (endline pos)
    ecol = (endcol pos)
    eoff = (endoff pos)
  in
    (pos { endcol = ecol - 1, endoff = eoff - 1 },
     pos { startline = eline, startcol = ecol, startoff = eoff })

-- | Create a position representing an internally-generated structure.
internal :: String -> Pos
internal fname = Pos { filename = fname, startline = -1, startcol = -1,
                       startoff = -1, endline = -1, endcol = -1,
                       endoff = -1 }

instance Position p => Position [p] where
  pos l = merge (pos (head l)) (pos (last l))

instance Hashable Pos where
  hash (Pos { startline = startline, startcol = startcol, startoff = startoff,
              endline = endline, endcol = endcol, endoff = endoff,
              filename = filename }) =
    hashInt startline `combine` hashInt startcol `combine`
    hashInt startoff `combine` hashInt endline `combine`
    hashInt endcol `combine` hashInt endoff `combine`
    hashFoldable filename

instance Ord Pos where
  compare p1 p2 =
    case compare (filename p1) (filename p2) of
      EQ -> case compare (startline p1) (startline p2) of
        EQ -> case compare (startcol p1) (startcol p2) of
          EQ -> case compare (endline p1) (endcol p2) of
            EQ -> compare (endcol p1) (endcol p2)
            out -> out
          out -> out
        out -> out
      out -> out

instance Format Pos where
  format p @ Pos { filename = fname, startline = -1, startcol = -1,
                   endline = -1, endcol = -1 } = text fname
  format p @ Pos { filename = fname, startline = sline, startcol = scol,
                   endline = eline, endcol = ecol }
    | sline == eline && scol == ecol =
      fname <+> sline <> "." <> scol
    | sline == eline =
      fname <+> sline <> "." <> scol <> "-" <> ecol
    | otherwise = fname <+> sline <> "." <> scol <> "-" <> eline <> "." <> ecol

instance Show Pos where
  show = show . format