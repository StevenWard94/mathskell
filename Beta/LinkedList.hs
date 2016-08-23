-- |
-- Module:        Beta.LinkedList
-- Author:        Steven Ward <stevenward94@gmail.com>
-- URL:           https://github.com/StevenWard94/mathskell
-- Last Change:   2016 Aug 22
--

module Beta.LinkedList where

import Control.Applicative
import Control.Monad

data Node a = Nul | Node a (Node a) deriving (Read)

instance Show a => Show (Node a) where
    show Nul        = "[| NUL |]"
    show (Node x _) = "[| " ++ show x ++ " |] → "

instance Eq a => Eq (Node a) where
    Nul == Nul              =  True
    Nul == _                =  False
    _   == Nul              =  False
    Node x n1 == Node y n2  =  x == y && n1 == n2

instance Ord a => Ord (Node a) where
    compare Nul Nul                =  EQ
    compare Nul _                  =  LT
    compare _   Nul                =  GT
    compare (Node x _) (Node y _)  =  x `compare` y

instance Functor Node where
    fmap _ Nul          =  Nul
    fmap f (Node x n')  =  Node (f x) $ fmap f n'

