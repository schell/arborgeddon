{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Arbor.Data.Types where

import Data.Maybe ( fromJust )

type Vector = [Float]

type Vec2 = (Float, Float)
type Vec3 = (Float, Float, Float)
type Vec4 = (Float, Float, Float, Float)


class Vectorize a where
    toVector   :: a -> Vector
    fromVector :: Vector -> Maybe a

class Vectorize a => Point a where
    distance :: a -> a -> a

class Point a => Line a where
    intersection :: (a, a) -> (a, a) -> a

class Vectorize a => Matrix a where
    -- | The identity matrix.
    identity    :: a
    -- | The number of columns in the matrix.
    numColumns  :: a -> Int
    -- | The number of rows in the matrix.
    numRows     :: a -> Int
    -- | A list of the columns.
    toColumns   :: a -> [[Float]]
    -- | A list of the rows.
    toRows      :: a -> [[Float]]
    -- | The minor for an element of `a` at the given row and column.
    minorAt     :: a -> Int -> Int -> Float

    -- | The matrix created by deleting column x and row y of the given matrix.
    deleteColRow :: a -> Int -> Int -> Vector
    deleteColRow m x y = let nRws = numRows m
                             nCls = numColumns m
                             rNdxs = [ row + x      | row <- [0,nCls..nCls*(nCls-1)] ]
                             cNdxs = [ nRws*y + col | col <- [0..nRws-1] ]
                             ndxs = rNdxs ++ cNdxs
                             (_, vec) = foldl filtNdx (0,[]) $ toVector m
                             filtNdx (i, acc) el = if i `elem` ndxs
                                                   then (i+1, acc)
                                                   else (i+1, acc++[el])
                         in vec
    -- | The transpose of the matrix.
    transpose   :: a -> a
    transpose = fromJust . fromVector . concat . toColumns
    -- | Computes the inverse of the matrix.
    inverse     :: a -> Maybe a
    inverse m = let det      = determinant m
                    one_det  = 1/ det
                    cofacts  = cofactors m
                    adjoint  = toVector $ transpose cofacts
                    Just inv = fromVector $ map (*one_det) adjoint
                in if det /= 0
                   then Just inv
                   else Nothing
    -- | The matrix of cofactors of the given matrix.
    cofactors   :: a -> a
    cofactors m = fromJust $ fromVector [ cofactorAt m x y | y <- [0..numRows m -1], x <- [0..numColumns m -1] ]
    -- | Computes the multiplication of two matrices.
    multiply    :: a -> a -> a
    multiply m1 m2 = let element row col = sum $ zipWith (*) row col
                         rows = toRows m1
                         cols = toColumns m2
                         vec  = [ element r c | r <- rows, c <- cols ]
                         Just m = fromVector vec
                     in m
    -- | The cofactor for an element of `a` at the given row and column.
    cofactorAt  :: a -> Int -> Int -> Float
    cofactorAt m x y = let pow = fromIntegral $ x + y + 2 -- I think zero indexed.
                       in (-1.0)**pow * minorAt m x y
    -- | Computes the determinant of the matrix.
    determinant :: a -> Float
    determinant m = let rowCofactors = [ cofactorAt m x 0 | x <- [0..numColumns m -1] ]
                        row = head $ toRows m
                    in sum $ zipWith (*) rowCofactors row

{- Default Vector based Matrix -}
determinantVec2x2 :: [Float] -> Float
determinantVec2x2 [a,b,c,d] = a*d - b*c
determinantVec2x2 _         = 0

