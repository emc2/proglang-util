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

-- | This module contains datatypes representing ranges of integers.
-- This is essentially a basis for interval arithmetic.
module Data.Interval(
       -- * Datatypes
       Interval(..),
       Intervals,

       -- * Constructors
       allNumbers,
       fromIntervalList,

       -- * Deconstructors
       toIntervalList,

       -- * Utility Functions
       size,
       span,
       distinctValues,

       -- * Pack/Unpack Offsets
       packOffsets,
       unpackOffsets
       ) where

import Control.Monad
import Data.Hashable
import Data.List hiding (span)
import Prelude hiding (span)

-- | A datatype representing a single interval
data Interval n =
    -- | An interval consisting of an inclusive range of numbers.
    -- Interval 1 2 contains both 1 and 2.
    Interval !n !n
    -- | An interval consisting of a single number.  Single x is
    -- equavalent to Interval x x.
  | Single !n
    -- | An interval (inclusively) lower-bounded by a number, with no
    -- upper-bound.  Min 0 denotes all positive integers, and zero.
  | Min !n
    -- | An interval (inclusively) upper-bounded by a number, with no
    -- lower-bound.  Max 0 denotes all negative integers, and zero.
  | Max !n
    deriving (Ord, Eq)

-- | A datatype representing a set of intervals.
newtype Intervals n = Intervals { intervals :: [Interval n] }
  deriving (Ord, Eq)

-- | Lower bound of a interval
lower :: Interval n -> n
lower (Single n) = n
lower (Interval n _) = n
lower (Min n) = n
lower (Max _) = error "Lower bound of a Max interval is undefined"

-- | Upper bound of a interval
upper :: Interval n -> n
upper (Single n) = n
upper (Interval _ n) = n
upper (Max n) = n
upper (Min _) = error "Upper bound of a Min interval is undefined"

-- | The Intervals object representing all numbers.
allNumbers :: Intervals n
allNumbers = Intervals { intervals = [] }

-- | Construct an Intervals object from a list of Interval objects.
-- The list may contain intervals that overlap, or are out of order.
fromIntervalList :: Integral n => [Interval n] -> Intervals n
fromIntervalList l = Intervals { intervals = normalizeInterval l }

-- | Convert an Intervals object to a sorted, normalized list of
-- Interval objects
toIntervalList :: Intervals n -> [Interval n]
toIntervalList (Intervals { intervals = l }) = l

normalizeInterval :: Integral n => [Interval n] -> [Interval n]
normalizeInterval =
  let
    -- Transform Interval n n into Single n
    collapse :: Eq n => Interval n -> Interval n
    collapse r @ (Interval n m)
           | n == m = Single n
           | otherwise = r
    collapse r = r

    -- reverse the order function to effectively reverse the lists
    orderInterval :: Ord n => Interval n -> Interval n -> Ordering
    orderInterval (Max n1) (Max n2) = compare n2 n1
    orderInterval (Max _) _ = GT
    orderInterval _ (Max _) = LT
    orderInterval r1 r2 = compare (lower r2) (lower r1)

    -- The actual normalization function, remember that the list is
    -- sorted in reverse order by the lower bound
    intervalNorm :: Integral n => [Interval n] -> [Interval n] -> [Interval n]
    -- If a min and max are adjacent, then there is either a
    -- "forbidden region", or else the integer is totally unbounded
    intervalNorm _ (Min minn : Max maxn : _)
            | minn > maxn + 1 = [Max maxn, Min minn]
            | otherwise = []
    -- This rule is necessary to avoid taking the upper bound of Min,
    -- which is undefined
    intervalNorm accum (_ : Min n : list) = intervalNorm accum (Min n : list)
    -- If a minimum overlaps another point, absorb it, otherwise
    -- discard all previous results and start over here.
    intervalNorm accum (Min n : r : list)
            | upper r >= n - 1 = intervalNorm accum (Min (lower r) : list)
            | otherwise = intervalNorm ([Min n]) (r : list)
    -- This rule is necessary to avoid taking the lower bound of Min,
    -- which is undefined
    intervalNorm accum (r : Max n : _) =
      intervalNorm (Max n : collapse r : accum) []
    -- Put the first Max on the end of the result list, then ignore
    -- everything that follows
    intervalNorm accum (Max n : _) = intervalNorm (Max n : accum) []
    -- Similar to the input list, max-min pairs generate an instant result
    intervalNorm (Max maxn : Min minn : _) []
            | minn > maxn + 1 = [Max maxn, Min minn]
            | otherwise = []
    -- Absorb a interval into the Max if it overlaps, otherwise stop
    intervalNorm result @ (Max n : r : accum) []
            | lower r <= n + 1 =
              intervalNorm (Max (max (upper r) n) : accum) []
            | otherwise = result
    -- The basic input list processing, with no mins or maxes.  If the
    -- two overlap, combine them.
    intervalNorm accum (r1 : r2 : list)
            | (lower r1) - 1 <= upper r2 =
              intervalNorm accum (Interval (lower r2) (upper r1) : list)
            | otherwise = intervalNorm (collapse r1 : accum) (r2 : list)
    intervalNorm accum [mono] = mono : accum
    -- Result lists that don't contain a Max don't need to be
    -- reprocessed
    intervalNorm accum [] = accum
  in
    intervalNorm [] . sortBy orderInterval

-- | Get the size of a single interval.
size :: Integral n => Interval n -> Maybe n
size (Interval lo hi) = Just (hi - lo + 1)
size (Single _) = Just 1
size _ = Nothing

-- | Get the number of distinct values that this Intervals object
-- represents.
distinctValues :: Integral n => Intervals n -> Maybe n
distinctValues = foldr (liftM2 (+)) (Just 0) . map size . toIntervalList

-- | Get the difference between the lowest and highest possible values
-- of an Intervals object.
span :: Intervals n -> Maybe (n, n)
span (Intervals { intervals = [] }) = Nothing
span (Intervals { intervals = is }) =
 case (head is, last is) of
      (Max _, _) -> Nothing
      (_, Min _) -> Nothing
      (firsti, lasti) -> Just (lower firsti, upper lasti)

-- | A possible list of (a, b) pairs, so that if x < a then x + b else
-- ...  will condense the integer into a single interval of values.  This
-- is useful for generating packing code.
packOffsets :: Integral n => Intervals n -> Maybe [(n, n)]
packOffsets =
  let
    genOffset (avail, Just list) (Single n) =
              (avail + 1, Just ((n, avail - n) : list))
    genOffset (avail, Just list) (Interval lo hi) =
              (avail + (hi - lo) + 1, Just ((hi, avail - lo) : list))
    genOffset (avail, _) _ = (avail, Nothing)
  in
    (liftM reverse) . snd . foldl genOffset (0, Just []) . toIntervalList

-- | A possible list of (a, b) pairs, so that if x < a then x + b else
-- ... will expand a condensed integer back out into its original
-- interval of values.  This is useful for generating unpacking code.
unpackOffsets :: Integral n => Intervals n -> Maybe [(n, n)]
unpackOffsets =
  let
    genOffset (avail, Just list) (Single n) =
      (avail + 1, Just ((avail, n - avail) : list))
    genOffset (avail, Just list) (Interval lo hi) =
      (avail + (hi - lo) + 1, Just ((avail, lo - avail) : list))
    genOffset (avail, _) _ = (avail, Nothing)
  in
    (liftM reverse) . snd . foldl genOffset (0, Just []) . toIntervalList

instance Show n => Show (Interval n) where
  show (Min n) = show n ++ " to +inf"
  show (Single n) = show n
  show (Interval n1 n2) = show n1 ++ " to " ++ show n2
  show (Max n) = "-inf to " ++ show n

instance Show n => Show (Intervals n) where
  show (Intervals { intervals = [] }) = "-inf to +inf"
  show (Intervals { intervals = is }) = show is

instance Hashable n => Hashable (Interval n) where
  hashWithSalt s (Interval n1 n2) =
    s `hashWithSalt` (1 :: Int) `hashWithSalt` n1 `hashWithSalt` n2
  hashWithSalt s (Single n) = s `hashWithSalt` (2 :: Int) `hashWithSalt` n
  hashWithSalt s (Min n) = s `hashWithSalt` (3 :: Int) `hashWithSalt` n
  hashWithSalt s (Max n) = s `hashWithSalt` (4 :: Int) `hashWithSalt` n

instance Hashable n => Hashable (Intervals n) where
  hashWithSalt s Intervals { intervals = is } = hashWithSalt s is
