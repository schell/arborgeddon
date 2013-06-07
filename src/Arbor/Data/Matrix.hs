module Arbor.Data.Matrix where

import Arbor.Data.Types
import Data.Maybe ( fromJust )

{- 2x2 -}
data Mat2x2 = Mat2x2 Float Float Float Float deriving (Show, Eq)

instance Vectorize Mat2x2 where
    toVector (Mat2x2 a b c d) = [a,b,c,d]
    fromVector [a,b,c,d]      = Just (Mat2x2 a b c d)
    fromVector _              = Nothing

instance Matrix Mat2x2 where
    zero = Mat2x2 0 0 0 0
    identity = Mat2x2 1 0 0 1
    numColumns _ = 2
    numRows _ = 2
    --cofactors (Mat2x2 a b c d) = Mat2x2 d (-c) (-b) a
    toColumns (Mat2x2 a b c d) = [[a,c], [b,d]]
    toRows    (Mat2x2 a b c d) = [[a,b], [c,d]]
    minorAt m x y = toVector m !! (y*2 + x)
    --cofactorAt m x y = toVector (cofactors m) !! (y*2 + x)

{- 3x3 -}
data Mat3x3 = Mat3x3 Float Float Float Float Float Float Float Float Float deriving (Show, Eq)

instance Vectorize Mat3x3 where
    toVector (Mat3x3 a b c d e f g h i) = [a,b,c,d,e,f,g,h,i]
    fromVector [a,b,c,d,e,f,g,h,i] = Just $ Mat3x3 a b c d e f g h i
    fromVector _ = Nothing

instance Matrix Mat3x3 where
    zero = Mat3x3 0 0 0 0 0 0 0 0 0
    identity = Mat3x3 1 0 0 0 1 0 0 0 1
    numColumns _ = 3
    numRows _ = 3
    toColumns (Mat3x3 a b c d e f g h i) = [[a,d,g],[b,e,h],[c,f,i]]
    toRows (Mat3x3 a b c d e f g h i) = [[a,b,c],[d,e,f],[g,h,i]]
    minorAt m x y = let row = toRows    m !! y
                        col = toColumns m !! x
                        exc = row ++ col
                        vec = toVector m
                        Just new = fromVector $ filter (`elem` exc) vec :: Maybe Mat2x2
                    in determinant new

-- type Mat4x4 = ( Float, Float, Float, Float
--               , Float, Float, Float, Float
--               , Float, Float, Float, Float
--               , Float, Float, Float, Float
--               )

