-- |
-- Module:        Physics.Basic
-- Author:        Steven Ward <stevenward94@gmail.com>
-- URL:           https://github.com/StevenWard94/mathskell
-- Last Change:   2016 Aug 03
--

module Physics.Basic
    (
      Vector(..)
    , magnitude
    ) where

import Control.Applicative

data Vector a = Zero | Unit | Vector a a

instance (Show a) => Show (Vector a) where
    show Zero = "(0,0)"
    show Unit = "(1,1)"
    show (Vector x y) = "(" ++ show x ++ "," ++ show y ++ ")"

instance (Real a) => Eq (Vector a) where
    (==) u v = case (u,v) of
                 (Zero,Zero) -> True
                 (Unit,Unit) -> True
                 (Zero,(Vector 0 0)) -> True
                 ((Vector 0 0),Zero) -> True
                 (Unit,(Vector 1 1)) -> True
                 ((Vector 1 1),Unit) -> True
                 ((Vector a b),(Vector c d)) -> a == c && b == d
                 _ -> False

instance (Real a) => Ord (Vector a) where
    compare u v = magnitude u `compare` magnitude v

instance Functor Vector where
    fmap f Zero = Zero
    fmap f Unit = undefined
    fmap f (Vector x y) = Vector (f x) (f y)

-- | NOTE: may not satisfy applicative functor laws!
instance Applicative Vector where
    pure s = Vector s s
    Zero <*> _ = Zero
    (Vector f g) <*> (Vector x y) = Vector (f x) (g y)

magnitude :: (Real a, Floating b) => Vector a -> b
magnitude Zero = 0.0
magnitude Unit = 1.0
magnitude (Vector x y) = sqrt . realToFrac $ x^2 + y^2
