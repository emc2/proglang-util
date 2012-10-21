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
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Polynomial.Class(
       Polynomial(..)
       ) where

class Polynomial e c t p where
  -- | The polynomial representation of zero.
  zero :: p c e t

  -- | The polynomial representation of one.
  one :: p c e t

  -- | Construct a polynomial from a number.
  fromNumber :: c
             -- ^ The number from which to build the polynomial.
             -> p c e t

  -- | If the polynomial is a constant, return its value.
  toNumber :: Monad m =>
              p c e t
              -- ^ A possibly constant polynomial.
              -> m c
              -- ^ Possibly the constant's value.

  -- | Construct a polynomial from its terms.
  fromTerms :: [(c, [(e, t)])] -> p c e t

  -- | Extract the terms from a polynomial.
  toTerms :: p c e t -> [(c, [(e, t)])]

  -- | Add one polynomial to another
  add :: p c e t -> p c e t -> p c e t

  -- | Negate a polynomial
  neg :: p c e t -> p c e t

  -- | Multiply one polynomial by another
  mult :: p c e t -> p c e t -> p c e t

  -- | Exponentiate a polynomial
  exp :: e -> p c e t -> p c e t

  -- | Expand a term in a polynomial into another polynomial
  expandOne :: t -> p c e t -> p c e t -> p c e t

  -- | Reduce a term in a polynomial to a number.
  -- 
  -- Note that if toNumber p0 == n, then
  -- expandOne t p0 p == reduceOne t (number p0) p
  reduceOne :: t -> c -> p c e t -> p c e t

  -- | Subtract one polynomial from another
  sub :: p c e t -> p c e t -> p c e t
  sub p1 = add p1 . neg