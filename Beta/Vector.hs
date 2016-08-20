-- |
-- Module:        Beta.Vector
-- Author:        Steven Ward <stevenward94@gmail.com>
-- URL:           https://github.com/StevenWard94/mathskell
-- Last Change:   2016 Aug 19
--

module Beta.Vector where

import Control.Applicative

data Vector a = Vector | a :. (Vector a)
infixr 5 :.

instance Show a => Show (Vector a) where
    show Vector       =  "( ø )"
    show (v:.Vector)  =  "( v )"
    show (v:.vs)      =  "( " ++ show v ++ ", " ++ showRest vs
        where showRest vs = case vs of
                                (v:.Vector) -> show v ++ " )"
                                (v:.vs)     -> show v ++ ", " ++ showRest vs

instance Eq a => Eq (Vector a) where
    (==) Vector Vector = True
    (==) (u:.Vector) (v:.Vector) = u == v
    (==) (x:.xs) (y:.ys)         = x == y && xs == ys
    (==)  _       _              = False

instance (Ord a, Num a) => Ord (Vector a) where
    compare = (. (sum . toList)) . compare . sum . toList

instance Functor Vector where
    fmap = (fromList .) . (. toList) . (<$>)

instance Applicative Vector where
    pure  = (:. Vector)
    vecVals  <*>  vecFuncs  =  fromList (zipWith ($) (toList vecVals) (toList vecFuncs))

instance Monoid (Vector a) where
    mempty  =  Vector
    mappend = (++.)

(++.) :: Vector a -> Vector a -> Vector a
(++.)  =  (fromList .) . (. toList) . (++) . toList

toList :: Vector a -> [a]
toList v  =  case v of
               Vector    -> []
               x:.Vector -> [x]
               (x:.xs)   ->  x : toList xs

fromList :: [a] -> Vector a
fromList []     = Vector
fromList [x]    = x:.Vector
fromList (x:xs) = x :. fromList xs
