{-# LANGUAGE CPP,
  Rank2Types,
  MultiParamTypeClasses,
  FlexibleContexts,
  TypeFamilies,
  ScopedTypeVariables, BangPatterns
  #-}

-- |
-- Module      : Mathskell.Practice
-- Maintainer  : Steven Ward <stevenward94@gmail.com>
-- Stability   : generally unstable
-- Last Change : 2016 July 21
--
-- Just a haskell script for messing around with different concepts,
-- modules, etc.
--

import Prelude hiding( length, (+), empty, (*) )
import qualified Prelude as P

infixr 5 :+:
data Vector a = Empty | Vec [a] | a :+: (Vector a) deriving (Show)

-- | zero and unit are inifinite-length representations of the zero- and
-- unit-vectors, respectively. zero' and unit' are functions that generate
-- finite-length zero- and unit-vectors, respectively.
zero :: Vector Int
zero = Vec (repeat 0)

unit :: Vector Int
unit = Vec (repeat 1)

zero' :: (Integral a) => a -> Vector Int
zero' n = Vec (take n zero)

unit' :: (Integral a) => a -> Vector Int
unit' n = Vec (take n unit)

-- | Getting the length/size (# of elements) of a Vector
length :: Vector a -> Int
length Empty = 0
length zero  = error "Error: zero is an infinite size - try zero' for a finite zero-vector"
length unit  = error "Error: unit is an infinite size - try unit' for a finite unit-vector"
length zero' = id
length unit' = id
length (Vec (x:xs)) = 1 P.+ length (Vec xs)
size = length

-- | Determine whether a Vector is Empty (calls length, so zero & unit will
-- throw errors
empty :: Vector a -> Bool
empty v = length v == 0

-- | Vector addition
infixl 6 +
(+) :: (Num a) => Vector a -> Vector a -> Vector a
(+) v Empty = v
(+) Empty = id
(+) v zero = v
(+) zero = id
(+) (Vec (x:xs)) unit = Vec (x P.+ 1 : xs + unit)
(+) (Vec (x:xs)) (Vec (y:ys)) = Vec (x P.+ y : xs + ys)

-- | Scalar multiplication with a vector (the 'dot' product)
infixl 7 *
(*) :: (Num a) => Int -> Vector a -> Vector a
(*) _ Empty = Empty
(*) _ zero = zero
(*) 0 _ = zero
(*) 1 = id
(*) c (Vec (x:xs)) = Vec (c P.* x : c * xs)
