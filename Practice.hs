-- |
--  Module:        Practice.hs
--  Author:        Steven Ward <stevenward94@gmail.com>
--  URL:           https://github.com/StevenWard94/mathskell
--  Last Change:   2016 July 24
--

import Prelude hiding( length, (+), (*) )
import qualified Prelude as P

infixr 5 :+:
data Vector = Empty | Zero | Unit | Int :+: Vector

-- | Implementing Vector as an instance of the Show typeclass
instance Show Vector where
  show Empty = ""
  show Zero = "{0}"
  show Unit = "{I}"
  show (x :+: xs) = show x ++ " " ++ show xs

-- | Generate a Vector from a list of Int values
fromList :: [Int] -> Vector
fromList [] = Empty
fromList [x] = x :+: Empty
fromList (x:xs) = x :+: fromList xs

-- | Getting the length/size (number of elements) of a Vector
length :: Vector -> Int
length Empty = 0
length Zero = error "{0} is of indeterminate length"
length Unit = error "{I} is of indeterminate length"
length (x :+: v) = 1 P.+ length v
size = length

-- | Operator for concatenating Vectors
infixr 5 .++
(.++) :: Vector -> Vector -> Vector
(.++) Empty v = v
(.++) _ Unit = error "cannot concatenate unit-vector"
(.++) Unit _ = error "cannot concatenate unit-vector"
(.++) _ Zero = error "cannot concatenate zero-vector"
(.++) Zero _ = error "cannot concatenate zero-vector"
(.++) v Empty = v
(x :+: xs) .++ v = x :+: (xs .++ v)

-- | Defining Vector addition - requires that Vectors have equal length
infixl 6 +
(+) :: Vector -> Vector -> Vector
(+) v Empty = v
(+) Empty v = v
(+) v Zero = v
(+) Zero v = v
(x:+:v) + Unit = (x P.+ 1) :+: (v + Unit)
Unit + (x:+:v) = (x P.+ 1) :+: (v + Unit)
(x:+:v) + (y:+:u) = (x P.+ y) :+: (v + u)
