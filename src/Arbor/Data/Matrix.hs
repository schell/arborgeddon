module Arbor.Data.Matrix where

import Arbor.Data.Types

{- 2x2 -}
data Mat2x2 = Mat2x2 Float Float Float Float deriving (Show, Eq)

instance Vectorize Mat2x2 where
    toVector (Mat2x2 a b c d) = [a,b,c,d]
    fromVector [a,b,c,d]      = Just (Mat2x2 a b c d)
    fromVector _              = Nothing

instance Matrix Mat2x2 where
    identity = Mat2x2 1 0 0 1
    numColumns _ = 2
    numRows _    = 2
    toColumns (Mat2x2 a b c d) = [[a,c], [b,d]]
    toRows    (Mat2x2 a b c d) = [[a,b], [c,d]]
    minorAt m x y = case deleteColRow m x y of
                        [minor] -> minor
                        minor   -> minorError minor m x y

{- 3x3 -}
data Mat3x3 = Mat3x3 Float Float Float Float Float Float Float Float Float deriving (Show, Eq)

instance Vectorize Mat3x3 where
    toVector (Mat3x3 a b c d e f g h i) = [a,b,c,d,e,f,g,h,i]
    fromVector [a,b,c,d,e,f,g,h,i] = Just $ Mat3x3 a b c d e f g h i
    fromVector _ = Nothing

instance Matrix Mat3x3 where
    identity = Mat3x3 1 0 0 0 1 0 0 0 1
    numColumns _ = 3
    numRows _    = 3
    toColumns (Mat3x3 a b c d e f g h i) = [[a,d,g],[b,e,h],[c,f,i]]
    toRows    (Mat3x3 a b c d e f g h i) = [[a,b,c],[d,e,f],[g,h,i]]
    minorAt m x y = let minorVec    = deleteColRow m x y
                        Just minorM = fromVector minorVec :: Maybe Mat2x2
                    in determinant minorM

{- Helpers -}
minorError :: (Show a, Show a1, Show a2, Show a3) => a3 -> a2 -> a -> a1 -> t
minorError minor m x y = error $ "The minorAt "++show x ++" "++show y++" of "++show m++" came back as "++show minor
