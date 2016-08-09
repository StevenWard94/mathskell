-- |
-- Module:        Beta.Vector
-- Author:        Steven Ward <stevenward94@gmail.com>
-- URL:           https://github.com/StevenWard94/mathskell
-- Last Change:   2016 Aug 09
--

module Beta.Vector
    (
      Vector(..)
    , sum, dim, (+>)
    , fromList, toList
    , (.+)
    ) where

import Prelude hiding ( sum )
import qualified Prelude as P

-- | Defining the Vector type
data Vector a = Null | a :> (Vector a)
infixr 5 :>

instance (Show a) => Show (Vector a) where
    show Null = "( )"
    show (x:>Null) = "( " ++ show x ++ " )"
    show (x:>xs) = "( " ++ show x ++ " " ++ showRest xs
        where showRest xs = case xs of
                              Null      -> " )"
                              (x:>Null) -> show x ++ " )"
                              (x:>xs)   -> show x ++ " " ++ showRest xs

instance (Eq a, Monoid a) => Eq (Vector a) where
    Null == Null = True
    xs   == ys   = if dim xs == dim ys
                    then sum xs == sum ys
                    else False

instance (Ord a, Monoid a) => Ord (Vector a) where
    compare Null Null = EQ
    compare Null _    = LT
    compare _    Null = GT
    xs `compare` ys   = if dim xs == dim ys
                          then sum xs `compare` sum ys
                          else dim xs `compare` dim ys

instance Functor Vector where
    fmap _ Null      = Null
    fmap f (x:>Null) = (f x):>Null
    fmap f (x:>xs)   = (f x) :> fmap f xs

instance Applicative Vector where
    {-# INLINE pure #-}
    pure x = x:>Null
    {-# INLINE (<*>) #-}
    fs <*> xs = fromList [ f x | x <- xlist, f <- flist ]
        where xlist = toList xs
              flist = toList fs
    {-# INLINE (*>) #-}
    xs *> ys = fromList [ y | _ <- toList xs, y <- toList ys]

instance Monoid (Vector a) where
    {-# INLINE mempty #-}
    mempty = Null
    {-# INLINE mappend #-}
    mappend = (+>)

instance Monad Vector where
    {-# INLINE return #-}
    return = pure
    {-# INLINE (>>=) #-}
    xs >>= f = fromList [ v | x <- toList xs, v <- toList $ f x ]

-- | Vector utility functions
sum :: (Monoid a) => Vector a -> a
sum Null = mempty
sum (x:>Null) = x
sum (x:>xs) = x `mappend` (sum xs)

dim :: Vector a -> Int
dim Null = 0
dim (x:>Null) = 1
dim (x:>xs) = 1 + dim xs

fromList :: [a] -> Vector a
fromList []     = Null
fromList [x]    = x:>Null
fromList (x:xs) = x :> fromList xs

toList :: Vector a -> [a]
toList Null      = []
toList (x:>Null) = [x]
toList (x:>xs)   = x : toList xs

zipWithV :: (a -> b -> c) -> Vector a -> Vector b -> Vector c
zipWithV _ Null _ = Null
zipWithV _ _ Null = Null
zipWithV f (x:>xs) (y:>ys) = (f x y) :> (zipWithV f xs ys)

(+>) :: Vector a -> Vector a -> Vector a
u +> v = fromList $ toList u ++ toList v
infixr 5 +>

-- | Implementations of arithmetic operators for the Vector type
(.+) :: (Real a) => Vector a -> Vector a -> Vector a
Null .+ y    = y
x    .+ Null = x
(x:>Null) .+ (y:>Null) = (x + y) :> Null
(x:>xs) .+ (y:>ys)
    | dim xs == dim ys = (x + y) :> (xs .+ ys)
    | otherwise       = undefined
infixl 6 .+

(.*) :: (Real a) => Vector a -> Vector a -> a
Null .* _    = 0
_    .* Null = 0
xs   .* ys
    | dim xs == dim ys = fromList $ foldr
