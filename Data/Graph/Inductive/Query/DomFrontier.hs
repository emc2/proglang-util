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
module Data.Graph.Inductive.Query.DomFrontier(
       domFrontiers
       ) where

import Control.Monad
import Data.Array.Unboxed
import Data.BitArray.IO
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Query.Dominators
import System.IO.Unsafe

-- This implementation uses bit arrays to avoid what would otherwise
-- be unnecessarily expensive operations.
domFrontiers' :: Graph gr => gr a b -> Node -> IO [(Node, [Node])]
domFrontiers' graph start =
  let
    range @ (start, end) = nodeRange graph
    indexes = [start..end]

    -- Use an unboxed array to get the immediate dominators
    idoms :: UArray Node Node
    idoms = array range (iDom graph start)

    -- We really have a two-dimensional array, but BitArray only lets
    -- us define one-dimensional arrays, so use this function to
    -- calculate the index.
    getIndex :: Node -> Node -> Int
    getIndex node elem =
      let
        node' = node - start
        elem' = elem - start
        span = end - start + 1
      in
        (node' * span) + elem'

    -- First index is the node, second index is whether or not that
    -- node is in the frontier of the first
    addToFrontier :: IOBitArray -> Node -> Node -> IO ()
    addToFrontier sets node elem =
      writeBit sets (getIndex node elem) True

    -- Run through all the bits corresponding to the argument's
    -- frontier, and produce an element of the frontier list.
    getFrontier :: IOBitArray -> Node -> IO (Node, [Node])
    getFrontier sets node =
      let
        foldfun :: [Node] -> Node -> IO [Node]
        foldfun front ind =
          do
            bit <- readBit sets (getIndex node ind)
            if bit
              then return (ind : front)
              else return front
      in do
        front <- foldM foldfun [] indexes
        return (node, front)

    -- Build the frontier for a node.  Algorithm by Ferrante.  There
    -- is another based on bottom-up traversal of a dominator tree.
    -- Discussion on the LLVMDev list suggests this algorithm actually
    -- performs better in most cases, as it does not construct a tree
    -- structure, and it skips "useless" nodes.
    buildFrontier :: IOBitArray -> Node -> IO ()
    buildFrontier sets node
      | indeg graph node <= 2 =
        let
          preds = pre graph node

          runback :: Node -> IO ()
          runback runner
            | node == start || runner /= idoms ! node =
              do
                addToFrontier sets runner node
                if runner /= start
                  then runback (idoms ! runner)
                  else return ()
            | otherwise = return ()
        in
          mapM_ runback preds
      | otherwise = return ()
  in do
    sets <- newBitArray ((getIndex start start),
                         (getIndex end end)) False
    mapM_ (buildFrontier sets) indexes
    mapM (getFrontier sets) indexes

-- | Calculate dominance frontiers for a graph, given a starting node.
--
-- This function works best if all the nodes IDs in the graph form a
-- contiguous range.  Sparse node IDs will result in poorer
-- performance.
domFrontiers :: Graph gr => gr a b -> Node -> [(Node, [Node])]
domFrontiers graph = unsafePerformIO . domFrontiers' graph