-- |
-- Module:        Beta.Vector
-- Author:        Steven Ward <stevenward94@gmail.com>
-- URL:           https://github.com/StevenWard94/mathskell
-- Last Change:   2016 July 27
--

module Beta.Vector
    (
      Vector(..)
    , Beta.Vector.null
    , dim
    ) where

import Prelude hiding( length, (*), (+) )
import qualified Prelude as P
import Data.Function
import Data.Char ( isDigit, isSpace )
import Data.List ( nub )
import Data.List.Split ( splitWhen )

data Vector = Zero | Unit | Vector [Int]

instance Show Vector where
  show Zero = "{ 0 }"
  show Unit = "{ 1 }"
  show (Vector []) = ""
  show (Vector [x]) = "( " ++ show x ++ " )"
  show (Vector (x:xs)) = "( " ++ show x ++ " " ++ showOthers xs ++ " )"
                      where
                        showOthers ys = case ys of
                            []      -> ""
                            [y]     -> show y
                            (y:ys)  -> show y ++ " " ++ showOthers ys

instance Read Vector where
  readsPrec _ input
    | P.null result = [(Vector [], "")]
    | nub result == [0] = [(Zero,"")]
    | nub result == [1] = [(Unit,"")]
    | otherwise = [(Vector result, "")]
    where
      result = map (read :: String -> Int) (filter (not . P.null) (splitWhen isSpace (filter (\c -> isDigit c || isSpace c) input)))

instance Eq Vector where
  u == v = case (u,v) of
             (Zero,Zero) -> True
             (Unit,Unit) -> True
             (Vector [],Vector []) -> True
             (Vector [x],Vector[y]) -> x == y
             (Vector (x:xs), Vector (y:ys))
                                          | dim u /= dim v -> False
                                          | otherwise -> (x == y) && (Vector xs == Vector ys)
             _ -> False

instance Ord Vector where
  compare (Vector []) _ = LT
  compare _ (Vector []) = GT
  compare Zero Zero = EQ
  compare Unit Unit = EQ
  compare Zero _ = LT
  compare _ Zero = GT
  compare Unit _ = LT
  compare _ Unit = GT
  u `compare` v = compare (dim u) (dim v)

null :: Vector -> Bool
null v = case v of
            (Vector []) -> True
            _           -> False

dim :: Vector -> Int
dim Zero = error "{ 0 } is of indefinite dimension"
dim Unit = error "{ 1 } is of indefinite dimension"
dim v = case v of
               Vector []     -> 0
               Vector [x]    -> 1
               Vector (x:xs) -> 1 P.+ dim (Vector xs)
