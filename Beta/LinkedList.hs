-- |
-- Module:        Beta.LinkedList
-- Author:        Steven Ward <stevenward94@gmail.com>
-- URL:           https://github.com/StevenWard94/mathskell
-- Last Change:   2016 Aug 22
--

module Beta.LinkedList where

import Control.Applicative
import Control.Monad

data Node a = Node a | Nul deriving (Read)

instance Show a => Show (Node a) where
    show (Node x) = "[|  data: " ++ show x ++ "  |] → "
    show Nul      = "[|  NUL  |]"

instance Eq a => Eq (Node a) where
    Nul == Nul        =  True
    Node x == Node y  =  x == y
    (==) _ _          = False

instance Ord a => Ord (Node a) where
    compare Nul Nul          =  EQ
    compare Nul _            =  LT
    compare _   Nul          =  GT
    Node x `compare` Node y  =  compare x y

instance Functor Node where
    fmap _ Nul      = Nul
    fmap f (Node x) = Node $ f x

instance Applicative Node where
    pure x           = Node x

    Node f <*> n       =  fmap f n
    Nul    <*> _n      =  Nul

    Node _n1 *> n2   =  n2
    Nul      *> _n2  =  Nul

instance Monad Node where
    (Node x) >>= f     =  f x
    Nul      >>= _     = Nul

    (>>) = (*>)
    fail _           = Nul

instance Alternative Node where
    empty = Nul
    Nul <|> r = r
    l   <|> _ = l

instance Monoid a => Monoid (Node a) where
    mempty = Nul
    Nul `mappend` n           =  n
    n `mappend` Nul           =  n
    Node n1 `mappend` Node n2 =  Node (n1 `mappend` n2)

