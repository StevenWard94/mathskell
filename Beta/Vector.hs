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
    , asList
    , asListWith
    , (+:)
    , (+:+)
    , vmap
    ) where

import Prelude hiding( length, (*), (+), null )
import qualified Prelude as P

import Data.Char ( isDigit, isSpace )
import Data.Function
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
dim Zero = undefined
dim Unit = undefined
dim v = case v of
               Vector []     -> 0
               Vector [x]    -> 1
               Vector (x:xs) -> 1 P.+ dim (Vector xs)

asList :: Vector -> [Int]
asList Zero = undefined
asList Unit = undefined
asList (Vector xs) = xs

asListWith :: (Int -> b) -> Vector -> [b]
asListWith _ Zero = undefined
asListWith _ Unit = undefined
asListWith f (Vector xs) = [ f x | x <- xs ]

infixr 5 +:
(+:) :: Int -> Vector -> Vector
(+:) _ Zero = undefined
(+:) _ Unit = undefined
(+:) x (Vector []) = Vector [x]
(+:) x (Vector [y]) = Vector [x,y]
(+:) x (Vector xs) = Vector (x:xs)

infixr 5 +:+
(+:+) :: Vector -> Vector -> Vector
(+:+) Zero _ = undefined
(+:+) _ Zero = undefined
(+:+) Unit _ = undefined
(+:+) _ Unit = undefined
(+:+) u v = Vector (asList u ++ asList v)

vmap :: (Int -> Int) -> Vector -> Vector
vmap _ Zero = Zero
vmap _ Unit = undefined
vmap f (Vector xs) = Vector [ f x | x <- xs ]

infixl 6 +
(+) :: Vector -> Vector -> Vector
(+) u Zero = u
(+) Zero v = v
(+) u Unit = vmap (P.+1) u
(+) Unit v = vmap (P.+1) v
(+) u v = if dim u == dim v
             then Vector $ zipWith (P.+) (asList u) (asList v)
             else undefined

-- | scalar multiplication; undefined when used on 2 Vectors
scalarX :: Either Int Vector -> Either Int Vector -> Vector
scalarX (Left _) (Right Zero) = Zero
scalarX (Right Zero) (Left _) = Zero
scalarX (Left c) (Right Unit) = Vector (repeat c)
scalarX (Right Unit) (Left c) = Vector (repeat c)
scalarX (Left c) (Right v) = vmap (P.*c) v
scalarX (Right u) (Left c) = vmap (P.*c) u
scalarX (Right _) (Right _) = undefined

-- | vector multiplication (cross-product)
infixl 7 #
(#) :: Vector -> Vector -> Int
(#) _ Zero = 0
(#) Zero _ = 0
(#) u Unit = sum $ asList u
(#) Unit v = sum $ asList v
(#) u v = if dim u == dim v
             then sum $ zipWith (P.*) (asList u) (asList v)
             else undefined
