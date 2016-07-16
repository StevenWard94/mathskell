{- | Definition of the Vector data type (IN PROGRESS)
 - Module:        Beta.Vector
 - Author:        Steven Ward <stevenward94@gmail.com>
 - URL:           https://github.com/StevenWard94/mathaskell
 - Last Change:   2016 July 14444
 -}

type Vector a = [a]

(*) :: Num a => Vector a -> Vector a -> a
