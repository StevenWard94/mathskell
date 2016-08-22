-- |
-- Module:        Beta.AltVector
-- Author:        Steven Ward <stevenward94@gmail.com>
-- URL:           https://github.com/StevenWard94/mathskell
-- Last Change:   2016 Aug 21
--

module Beta.AltVector where

import Data.Foldable ( toList )
import Control.Monad

data Vector a  =  Vector [a]
    deriving (Read, Eq, Ord)

instance Show a => Show (Vector a) where
    show (Vector [])      =  "( )"
    show (Vector [x])     =  "( " ++ show x ++ " )"
    show (Vector (x:xs))  =  "( " ++ show x ++ ", " ++ showTail xs ++ " )"
        where showTail ys = case ys of
                                (z:zs)  ->  show z ++ ", " ++ showTail zs
                                [z]     -> show z

instance Functor Vector where
    fmap g (Vector xs)  =  Vector (map g xs)

instance Applicative Vector where
    pure  =  return
    Vector [] <*> _  =  Vector []
    _ <*> Vector []  =  Vector []
    Vector fs <*> Vector xs  = case (fs,xs) of
                                 ((g:gs), (y:ys)) -> [g y] ++> (Vector gs <*> Vector ys)
                                 ([g], (y:ys))    -> Vector [g y]
                                 ((g:gs), [y])    -> Vector [g y]

instance Monad Vector where
    return  =  Vector . return
    Vector [] >>= _   =  Vector []


instance Monoid (Vector a) where
    mempty  =  Vector []
    mappend  =  (++.)

(++.) :: Vector a -> Vector a -> Vector a
Vector xs ++. Vector ys  =  Vector (xs ++ ys)
infixr 5 ++.

concatV :: Vector (Vector a) -> Vector a
concatV  =  fromList . (vToList =<<) . vToList


(++>) :: [a] -> Vector a -> Vector a
xs ++> Vector ys  =  Vector (xs ++ ys)
infixr 5 ++>

headV :: Vector a -> a
headV  =  head . vToList

tailV :: Vector a -> Vector a
tailV  =  fromList . tail . vToList

push :: a -> Vector a -> Vector a
push x (Vector xs)  =  Vector (x:xs)

pop :: Vector a -> (a,Vector a)
pop  =  liftM2 (,) headV tailV

fromList :: [a] -> Vector a
fromList []   =  Vector []
fromList [x]  =  Vector [x]
fromList xs   =  Vector xs

fromFoldable :: (Foldable t) => t a -> Vector a
fromFoldable  =  fromList . toList

vToList :: Vector a -> [a]
vToList (Vector xs)  =  xs
