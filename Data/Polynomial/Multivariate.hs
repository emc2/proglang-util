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
{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | This module contains an implementation of multivariate
-- polynomials, with basic instances and mathematical operations.  At
-- the present, this uses the (likely inefficient) implementation of
-- rings in NumericPrelude.
module Data.Polynomial.Multivariate(
       Polynomial
       ) where

import Control.Applicative
import Data.Array(Array)
import Data.Foldable
import Data.Map(Map)
import Data.List(intercalate)
import Data.Traversable
import Data.Word
import NumericPrelude hiding (concat, mapM, sequence,
                              foldr, foldl, foldr1, foldl1)

import qualified Data.Array as Array
import qualified Data.Map as Map
import qualified MathObj.Algebra as Algebra
import qualified Algebra.Additive as Additive
import qualified Algebra.Monoid as Monoid
import qualified Algebra.Ring as Ring

-- | An internal type for monomials, parameterizable on the exponent.
data Monomial e = M (Map Word e)

instance Functor Monomial where
  fmap f (M m) = M (fmap f m)

instance (Eq e, Additive.C e) => Monoid.C (Monomial e) where
  idt = M Map.empty
  (M m1) <*> (M m2) =
    M (Map.filter (/= zero) (Map.unionWith (+) m1 m2))

instance Show e => Show (Monomial e) where
  show (M m) = concat (map (\(k, e) -> "x_" ++ show k ++ "^" ++ show e)
                           (Map.toDescList m))

instance Ord e => Ord (Monomial e) where
  compare (M m1) (M m2) = compare (Map.toDescList m1) (Map.toDescList m2)

instance Eq e => Eq (Monomial e) where
  (M m1) == (M m2) = m1 == m2

-- | Multivariate polynomials.  Uses a map to only store one copy of
-- the term in question, and uses indexes to refer to it throughout
-- the actual algebraic structure.
data Polynomial c e t = P (Algebra.T (Monomial e) c) (Array Word t)

instance (Show t, Show c, Show e) => Show (Polynomial t c e) where
  show (P alg terms) =
    show alg ++ " where " ++
      intercalate ", " (map (\(k, t) -> "x_" ++ show k ++ " = " ++ show t)
                            (Array.assocs terms))

-- Deal with the politics of remapping variables, when combining two
-- polynomials with a binary function.
combine :: (Ord t, Ord e) =>
           (Algebra.T (Monomial e) c -> Algebra.T (Monomial e) c ->
            Algebra.T (Monomial e) c) ->
           Polynomial c e t -> Polynomial c e t -> Polynomial c e t
combine op (P (Algebra.Cons map1) t1) (P (Algebra.Cons map2) t2) =
  let
    mapMonoVars :: (Word -> Word) -> Monomial e -> Monomial e
    mapMonoVars f (M m) = M (Map.mapKeys f m)

    -- Merge two sorted arrays of terms, mapping words to terms.
    -- Produce a merged array, and two mappings.
    merge :: Ord a => Array Word a -> Array Word a ->
             (Array Word a, Array Word Word, Array Word Word)
    merge a1 a2 =
      let
        merge' :: Ord a => ([a], [Word], [Word], Word) -> [a] -> [a] ->
                           (Array Word a, Array Word Word, Array Word Word)
        merge' (merged, amap, bmap, ind) (a : as) (b : bs) =
          case compare a b of
            LT -> merge' (a : merged, ind : amap, bmap, ind + 1) as (b : bs)
            GT -> merge' (b : merged, amap, ind : bmap, ind + 1) (a : as) bs
            EQ -> merge' (a : merged, ind : amap, ind : bmap, ind + 1) as bs
        merge' (merged, amap, bmap, ind) (a : as) [] =
          merge' (a : merged, ind : amap, bmap, ind + 1) as []
        merge' (merged, amap, bmap, ind) [] (b : bs)=
          merge' (b : merged, amap, ind : bmap, ind + 1) [] bs
        merge' (merged, amap, bmap, ind) [] [] =
          (Array.listArray (0, ind) (reverse merged),
           Array.listArray (0, (fromIntegral (length amap)) - 1) (reverse amap),
           Array.listArray (0, (fromIntegral (length bmap)) - 1) (reverse bmap))
      in
        merge' ([], [], [], 0) (Array.elems a1) (Array.elems a2)

    -- Merge the arrays
    (mergedarr, rename1, rename2) = merge t1 t2
    -- Rename the variables in
    p1 = Algebra.Cons (Map.mapKeys (mapMonoVars (rename1 Array.!)) map1)
    p2 = Algebra.Cons (Map.mapKeys (mapMonoVars (rename2 Array.!)) map2)
  in
    P (op p1 p2) mergedarr

instance (Ord c, Ord e, Ord t, Additive.C c, Additive.C e) =>
         Additive.C (Polynomial c e t) where
  zero = P zero (Array.listArray (1, 0) [])
  negate (P alg terms) = P (negate alg) terms
  (+) = combine (+)
  (-) = combine (-)

instance (Ord c, Ord e, Ord t, Ring.C c, Additive.C e) =>
         Ring.C (Polynomial c e t) where
  one = P one (Array.listArray (1, 0) [])
  fromInteger n = P (fromInteger n) (Array.listArray (1, 0) [])
  (P alg terms) ^ e = P (alg ^ e) terms
  (*) = combine (*)

instance Functor (Polynomial c e) where
  fmap f (P alg terms) = P alg (fmap f terms)

instance Foldable (Polynomial c e) where
  fold (P _ terms) = fold terms
  foldMap f (P _ terms) = foldMap f terms
  foldr f i (P _ terms) = foldr f i terms
  foldl f i (P _ terms) = foldl f i terms
  foldr1 f (P _ terms) = foldr1 f terms
  foldl1 f (P _ terms) = foldl1 f terms

instance Traversable (Polynomial c e) where
  traverse f (P alg terms) = P alg <$> traverse f terms
  sequenceA (P alg terms) = P alg <$> sequenceA terms
  mapM f (P alg terms) = mapM f terms >>= return . P alg
  sequence (P alg terms) = sequence terms >>= return . P alg
{-
instance Applicative (Polynomial c e) where
  pure x = P (Array.listArray (0, 0) [x])
             (Algebra.monomial (Map.singleton 1 one) one)

-}

instance Monad (Polynomial c e) where
  return x = P (Algebra.monomial (Map.singleton 1 one) one)
               (Array.listArray (0, 0) [x])
  (P alg terms) >>= f = mapM f terms >>= return . P alg