-- |
-- Module:        Beta.Vector
-- Author:        Steven Ward <stevenward94@gmail.com>
-- URL:           https://github.com/StevenWard94/mathskell
-- Last Change:   2016 July 25
--

data Vector = Zero | Unit | Vector [Int]
type Empty = Vector []

instance Show Vector where
  show Empty = "( )"
  show Zero = "{ 0 }"
  show Unit = "{ 1 }"
  show (Vector (x:xs)) = "( " ++ show x ++ " " ++ show (Vector xs) ++ " )"
