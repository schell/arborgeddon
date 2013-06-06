module Arbor.Data.Types where

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
    -- | The zero matrix.
    zero        :: a
    -- | The identity matrix.
    identity    :: a
    -- | The transpose of the matrix.
    transpose   :: a -> a
    -- | Computes the inverse of the matrix.
    inverse     :: a -> Maybe a
    -- | The matrix of cofactors of the given matrix.
    cofactors   :: a -> a
    -- | Computes the multiplication of two matrices.
    multiply    :: a -> a -> a
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
    -- | The cofactor for an element of `a` at the given row and column.
    cofactorAt  :: a -> Int -> Int -> Float
    -- | Computes the determinant of the matrix.
    determinant :: a -> Float

