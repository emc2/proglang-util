module Data.Graph.Inductive.Query.DomFrontier(
       domFrontiers
       ) where

import Control.Monad
import Data.Array.Unboxed
import Data.BitArray.IO
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Query.Dominators
import System.IO.Unsafe

domFrontiers' :: Graph gr => gr a b -> Node -> IO [(Node, [Node])]
domFrontiers' graph start =
  let
    range @ (start, end) = nodeRange graph
    indexes = [start..end]

    idoms :: UArray Node Node
    idoms = array range (iDom graph start)

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

domFrontiers :: Graph gr => gr a b -> Node -> [(Node, [Node])]
domFrontiers graph = unsafePerformIO . domFrontiers' graph