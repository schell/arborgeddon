{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Geometry.Matrix where

import Geometry.Vector

import Data.Maybe       ( fromJust )

import qualified Data.List as L

type Matrix = [[Float]]

instance Vectorize Matrix where
    toVector = concat
    fromVector v = let numCols = floor $ sqrt $ fromIntegral $ length v
                   in Just $ groupByRowsOf numCols v



-- | The identity of the given matrix.
identity :: Matrix -> Matrix
identity m = groupByRowsOf rows modList
    where modList = [ if x `mod` (cols+1) == 0 then 1 else 0 | x <- [0..len-1] ]
          len     = sum $ map length m
          rows    = numRows m
          cols    = numColumns m
-- | The number of columns in the matrix.
numColumns :: Matrix -> Int
numColumns = length
-- | The number of rows in the matrix.
numRows :: Matrix -> Int
numRows []    = 0
numRows (r:_) = length r
-- | A list of the columns.
toColumns :: Matrix -> [[Float]]
toColumns = transpose
-- | A list of the rows.
toRows :: Matrix -> [[Float]]
toRows = id
-- | The minor for an element of `a` at the given row and column.
minorAt     :: Matrix -> Int -> Int -> Float
minorAt m x y = let del = deleteColRow m x y
                in determinant del
-- | The Matrix created by deleting column x and row y of the given matrix.
deleteColRow :: Matrix -> Int -> Int -> Matrix
deleteColRow m x y = let nRws = numRows m
                         nCls = numColumns m
                         rNdxs = [ row + x      | row <- [0,nCls..nCls*(nCls-1)] ]
                         cNdxs = [ nRws*y + col | col <- [0..nRws-1] ]
                         ndxs = rNdxs ++ cNdxs
                         (_, vec) = foldl filtNdx (0,[]) $ toVector m
                         filtNdx (i, acc) el = if i `elem` ndxs
                                               then (i+1, acc)
                                               else (i+1, acc++[el])
                     in groupByRowsOf (nRws-1) vec
-- | The transpose of the matrix.
transpose   :: Matrix -> Matrix
transpose = L.transpose
-- | Computes the inverse of the matrix.
inverse     :: Matrix -> Maybe Matrix
inverse m = let det      = determinant m
                one_det  = 1/ det
                cofacts  = cofactors m
                adjoint  = transpose cofacts
                inv      = (map . map) (*one_det) adjoint
            in if det /= 0
               then Just inv
               else Nothing
-- | The matrix of cofactors of the given matrix.
cofactors   :: Matrix -> Matrix
cofactors m = fromJust $ fromVector [ cofactorAt m x y | y <- [0..numRows m -1], x <- [0..numColumns m -1] ]
-- | Computes the multiplication of two matrices.
multiply    :: Matrix -> Matrix -> Matrix
multiply m1 m2 = let element row col = sum $ zipWith (*) row col
                     rows = toRows m1
                     cols = toColumns m2
                     vec  = [ element r c | r <- rows, c <- cols ]
                     Just m = fromVector vec
                 in m
-- | The cofactor for an element of `a` at the given row and column.
cofactorAt  :: Matrix -> Int -> Int -> Float
cofactorAt m x y = let pow = fromIntegral $ x + y + 2 -- I think zero indexed.
                   in (-1.0)**pow * minorAt m x y
-- | Computes the determinant of the matrix.
determinant :: Matrix -> Float
determinant [[a]] = a
determinant m  = let rowCofactors = [ cofactorAt m x 0 | x <- [0..numColumns m -1] ]
                     row = head $ toRows m
                 in sum $ zipWith (*) rowCofactors row

{- Helpers -}
groupByRowsOf :: Int -> [a] -> [[a]]
groupByRowsOf _    [] = []
groupByRowsOf cols xs = take cols xs : groupByRowsOf cols (drop cols xs)

