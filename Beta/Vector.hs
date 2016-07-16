{-# LANGUAGE CPP
          , DeriveDataTypeable
          , FlexibleInstances
          , MultiParamTypeClasses
          , Rank2Types
          , BangPatterns
  #-}

-- |
-- Module:        Beta.Vector
-- Author:        Steven Ward <stevenward94@gmail.com>
-- URL:           https://github.com/StevenWard94/mathaskell
-- Last Change:   2016 July 15
--
-- My implementation of an Int Vector data type (IN PROGRESS)
-- NOTE: this uses A LOT of help from the Data.Vector module already
-- available through hackage
--

module Beta.Vector (
  -- * Vector type
  Vector,

  -- * Accessors

  -- ** Length information
  length, null,

  -- ** Indexing
  (!), (!?), head, last,
  unsafeIndex, unsafeHead, unsafeLast,

  -- ** Monadic Indexing
  -- indexM, headM, lastM,
  -- unsafeIndexM, unsafeHeadM, unsafeLastM,

  -- ** Extracting subvectors (slicing)
  slice, init, tail, take, drop, splitAt,
  unsafeSlice, unsafeInit, unsafeTail, unsafeTake, unsafeDrop,

  -- * Construction

  -- ** Initialization
  empty, singleton, replicate, generate, iterateN,

  -- ** Monadic initialization
  -- replicateM, generateM, create,

  -- ** Unfolding
  unfoldr, unfoldrN, constructN, constructrN,

  -- ** Enumeration
  enumFromN, enumFromStepN, enumFromTo, enumFromThenTo,

  -- ** Concatenation
  cons, snoc, (++), concat,

  -- ** Restricting memory usage
  force,

  -- * Modifying Vectors

  -- ** Bulk updates
  (//), update, update_,
  unsafeUpd, unsafeUpdate, unsafeUpdate_,

  -- ** Accumulations
  accum, accumulate, accumulate_,
  unsafeAccum, unsafeAccumulate, unsafeAccumulate_,

  -- ** Permutations
  reverse, backpermute, unsafeBackpermute,

  -- ** Safe destructive updates
  modify,

  -- * Elementwise operations

  -- ** Indexing
  indexed,

  -- ** Mapping
  map, imap, concatMap,

  -- ** Monadic mapping
  -- mapM, imapM, mapM_, imapM_, forM, forM_,

  -- ** Zipping
  zipWith, zipWith3, zipWith4, zipWith5, zipWith6,
  izipWith, izipWith3, izipWith4, izipWith5, izipWith6,
  zip, zip3, zip4, zip5, zip6,

  -- ** Monadic zipping
  -- zipWithM, izipWithM, zipWithM_,izipWithM_,

  -- ** Unzipping
  unzip, unzip3, unzip4, unzip5, unzip6,

  -- * Working with predicates

  -- ** Filtering
  filter, ifilter, filterM, takeWhile, dropWhile,

  -- ** Partitioning
  partition, unstablePartition, span, break,

  -- ** Searching
  elem, notElem, find, findIndex, findIndices, elemIndex, elemIndices,

  -- * Folding
  foldl, foldl1, foldl', foldl1', foldr, foldr1, foldr', foldr1',
  ifoldl, ifoldl', ifoldr, ifoldr',

  -- ** Specialized folding
  all, any, and, or,
  sum, product,
  maximum, maximumBy, minimum, minimumBy,
  minIndex, minIndexBy, maxIndex, maxIndexBy,

  -- ** Monadic folds
  -- foldM, ifoldM, foldM', ifoldM',
  -- fold1M, fold1M', foldM_, ifoldM_,
  -- foldM'_, ifoldM'_, fold1M_, fold1M'_,

  -- ** Monadic sequencing
  -- sequence, sequence_,

  -- * Prefix sums (scans)
  prescanl, prescanl', postscanl, postscanl',
  scanl, scanl', scanl1, scanl1',
  prescanr, prescanr', postscanr, postscanr',
  scanr, scanr', scanr1, scanr1',

  -- * Conversions

  -- ** Lists
  toList, fromList, fromListN,

  -- ** Other Vector types
  -- G.convert,

  -- ** Mutable vectors
  -- freeze, thaw, copy, unsafeFreeze, unsafeThaw, unsafeCopy
) where

import qualified Data.Vector.Generic as G
import           Data.Vector.Mutable  ( MVector(..) )
import           Data.Primitive.Array
import qualified Data.Vector.Fusion.Bundle as Bundle

import Control.DeepSeq ( NFData, rnf )
import Control.Monad ( MonadPlus(..), liftM, ap )
import Control.Monad.ST ( ST )
import Control.Monad.Primitive

import Prelude hiding ( length, null,
                        replicate, (++), concat,
                        head, last,
                        init, tail, take, drop, splitAt, reverse,
                        map, concatMap,
                        zipWith, zipWith3, zip, zip3, unzip, unzip3,
                        filter, takeWhile, dropWhile, span, break,
                        elem, notElem,
                        foldl, foldl1, foldr, foldr1,
                        all, any, and, or, sum, product, minimum, maximum,
                        scanl, scanl1, scanr, scanr1,
                        enumFromTo, enumFromThenTo,
                        mapM, mapM_, sequence, sequence_ )

import Data.Typeable ( Typeable )
import Data.Data     ( Data(..) )
import Text.Read     ( Read(..), readListPrecDefault )

import Data.Monoid   ( Monoid(..) )
import qualified Control.Applicative as Applicative
import qualified Data.Foldable as Foldable
import qualified Data.Traversable as Traversable

#if __GLASGOW_HASKELL__ >= 708
    import qualified GHC.Exts as Exts (IsList(..))
#endif

-- | Boxed Vectors
data Vector a = Vector {-# UNPACK #-} !Int
                       {-# UNPACK #-} !Int
                       {-# UNPACK #-} !(Array a)
        deriving ( Typeable )

instance NFData a => NFData (Vector a) where
    rnf (Vector i n arr) = rnfAll i
        where
          rnfAll ix | ix < n    = rnf (indexArray arr ix) `seq` rnfAll (ix+1)
                    | otherwise = ()

instance Show a => Show (Vector a) where
    showsPrec = G.showsPrec
