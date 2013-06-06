module Arbor.Data.Matrix where

import Arbor.Data.Types

-- | Computes the determinant of the given 2x2 matrix.
-- determinant2x2 :: Mat2x2 -> Float
-- determinant2x2 (a,b,c,d) = a*d - b*c
data Mat2x2 = Mat2x2 Float Float Float Float deriving (Show, Eq)

instance Vectorize Mat2x2 where
    toVector (Mat2x2 a b c d) = [a,b,c,d]
    fromVector [a,b,c,d]      = Just (Mat2x2 a b c d)
    fromVector _              = Nothing

instance Matrix Mat2x2 where
    identity                     = Mat2x2 1 0 0 1
    transpose   (Mat2x2 a b c d) = Mat2x2 a c b d
    inverse   m@(Mat2x2 a b c d) = let det      = 1/ determinant m
                                       flp      = [d, -1*b, -1*c, a]
                                       vec      = map (*det) flp 
                                       Just inv = fromVector vec
                                   in inv 
    cofactors                    = undefined
    multiply                     = undefined
    numColumns _                 = 2
    numRows _                    = 2
    minorAt                      = undefined
    cofactorAt                   = undefined
    determinant (Mat2x2 a b c d) = a*d - b*c    

-- type Mat3x3 = ( Float, Float, Float
--               , Float, Float, Float
--               , Float, Float, Float
--               )
-- type Mat4x4 = ( Float, Float, Float, Float
--               , Float, Float, Float, Float
--               , Float, Float, Float, Float
--               , Float, Float, Float, Float
--               )

