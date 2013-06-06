module Arbor.Data.Matrix where

import Arbor.Data.Types
import Test.QuickCheck
import Debug.Trace

-- | Computes the determinant of the given 2x2 matrix.
-- determinant2x2 :: Mat2x2 -> Float
-- determinant2x2 (a,b,c,d) = a*d - b*c
data Mat2x2 = Mat2x2 Float Float Float Float deriving (Show, Eq)

instance Vectorize Mat2x2 where
    toVector (Mat2x2 a b c d) = [a,b,c,d]
    fromVector [a,b,c,d]      = Just (Mat2x2 a b c d)
    fromVector _              = Nothing

instance Matrix Mat2x2 where
    zero = Mat2x2 0 0 0 0
    identity = Mat2x2 1 0 0 1
    transpose (Mat2x2 a b c d) = Mat2x2 a c b d
    inverse m@(Mat2x2 a b c d) = let det      = determinant m
                                     one_det  = 1/ det
                                     flp      = [d, -b, -c, a]
                                     vec      = map (*one_det) flp
                                     Just inv = fromVector vec
                                 in if det /= 0
                                    then Just inv
                                    else Nothing
    cofactors (Mat2x2 a b c d) = Mat2x2 d (-c) (-b) a
    multiply (Mat2x2 a1 b1 c1 d1) (Mat2x2 a2 b2 c2 d2) = Mat2x2 (a1*a2+b1*c2) (a1*b2+b1*d2) (c1*a2+d1*c2) (c1*b2+d1*d2)
    numColumns _ = 2
    numRows _ = 2
    toColumns (Mat2x2 a b c d) = [[a,c], [b,d]]
    toRows    (Mat2x2 a b c d) = [[a,b], [c,d]]
    minorAt m x y = toVector m !! (y*2 + x)
    cofactorAt m x y = toVector (cofactors m) !! (y*2 + x)
    determinant (Mat2x2 a b c d) = a*d - b*c

instance Arbitrary Mat2x2 where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        return $ Mat2x2 a b c d
    shrink = shrink



-- type Mat3x3 = ( Float, Float, Float
--               , Float, Float, Float
--               , Float, Float, Float
--               )
-- type Mat4x4 = ( Float, Float, Float, Float
--               , Float, Float, Float, Float
--               , Float, Float, Float, Float
--               , Float, Float, Float, Float
--               )

