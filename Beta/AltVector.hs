-- |
-- Module:        Beta.AltVector
-- Author:        Steven Ward <stevenward94@gmail.com>
-- URL:           https://github.com/StevenWard94/mathskell
-- Last Change:   2016 Aug 22
--

module Beta.AltVector where

import Control.Monad
import Data.Foldable

import qualified Data.List as List

data Vector a  =  Vector [a]
    deriving (Read, Eq, Ord)

instance Show a => Show (Vector a) where
    show (Vector [])      =  "( )"
    show (Vector [x])     =  "( " ++ show x ++ " )"
    show (Vector (x:xs))  =  "( " ++ show x ++ ", " ++ showTail xs ++ " )"
        where showTail ys = case ys of
                                (z:zs)  ->  show z ++ ", " ++ showTail zs
                                [z]     -> show z

instance Foldable Vector where
    foldr f z (Vector xs)  =  List.foldr f z xs

instance Functor Vector where
    fmap  = (fromFoldable .) . (. toList) . map

instance Applicative Vector where
    pure = return
    Vector [] <*> _ = Vector []
    _ <*> Vector [] = Vector []
    Vector fs <*> Vector vs = Vector $ fs <*> vs


fromFoldable :: (Foldable t) => t a -> Vector a
fromFoldable = Vector . toList
