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

data Vector = Empty | Vector [a] deriving (Show)

-- | get the length of a vector
length :: Vector a -> Int
length Empty = 0
length (Vector (x:xs)) = 1 + length xs

size = length

-- | check if a vector is empty
empty :: Vector a -> Bool
empty Empty = True
empty (Vector [a]) = False
