-- |
-- Module:        Physics.Vector.Basic
-- Author:        Steven Ward <stevenward94@gmail.com>
-- URL:           https://github.com/StevenWard94/mathskell
-- Last Change:   2016 Sep 01
--

-- TODO: specify an explicit export list for the module
module Physics.Vector.Basic where

import Control.Applicative
import Data.Bits ( xor )

type Magnitude a = a
newtype Direction a = Direction (a,a)
    deriving (Read, Eq, Bounded)

getDir :: Direction a -> (a,a)
getDir (Direction xy) = xy

data Vector2D a b = Vector2D (Direction a) (Magnitude a)
    deriving (Read)

--
-- Instance declarations for the Direction newtype
--

instance (Num a, Show a) => Show (Direction a) where
    show (Direction (x,y)) = "(" ++ show x ++ ", " ++ show y ++ ")"

instance (Num a, Ord a) => Ord (Direction a) where
    Direction (x,y) `compare` Direction (x',y')
      | x == x' && y == y'         = EQ
      | x > x' && y > y'         = GT
      | (x > x') `xor` (y > y') = (x + y) `compare` (x' + y')
      | otherwise               = LT

instance Functor Direction where
    fmap = dirMap

instance Applicative Direction where
    pure x = Direction (x,x)
    Direction (f,g) <*> Direction (x,y) = runDir (f x) (g y)

--
-- Functions for using/manipulating Direction newtypes (and to a lesser extent,
-- Magnitude types)
--

getX :: (Num a) => Direction a -> a
getX = fst . getDir

getY :: (Num a) => Direction a -> a
getY = snd . getDir

dirMap :: (a -> b) -> Direction a -> Direction b
dirMap f (Direction (x,y)) = Direction (f x, f y)

runDir :: a -> a -> Direction a
runDir = (Direction .) . (,)
