-- |
-- Module:        Physics.Vector.Basic
-- Author:        Steven Ward <stevenward94@gmail.com>
-- URL:           https://github.com/StevenWard94/mathskell
-- Last Change:   2016 Sep 01
--

module Physics.Vector.Basic where

newtype Direction a = Direction { getDirection :: (a,a) }

data Vector2D a b  =  Vector2D (Direction a) b
    deriving (Read)

