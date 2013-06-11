module Geometry.Vector (
    Vector,
    Vec2,
    Vec3,
    Vec4,
    magnitude,
    normalize,
    unitize,
    add,
    subtract,
    vecNAt,
    vec2At,
    vec3At,
    vec4At
) where

import Prelude hiding ( subtract )

{- Vector Types -}
type Vector a = [a]

type Vec2 = (Float, Float)
type Vec3 = (Float, Float, Float)
type Vec4 = (Float, Float, Float, Float)

{- Vector functions -}
-- | Computes the magnitude.
magnitude :: Floating a => Vector a -> a
magnitude = sqrt . sum . map (**2)

-- | Computes the unit vector.
normalize :: Floating a => [a] -> [a]
normalize vec = map (/mag) vec
    where mag = magnitude vec

-- | Computes the unit vector.
unitize :: Floating a => [a] -> [a]
unitize = normalize

-- | Adds two vectors.
add :: Floating a => [a] -> [a] -> [a]
add = zipWith (+)

-- | Subtracts two vectors.

subtract :: Num a => [a] -> [a] -> [a]
subtract = zipWith (-)

-- | Returns the a'th vector of n components in the given vector.
vecNAt :: Int -> Int -> [a] -> [a]
vecNAt n a v  = let abn = abs n
                    aba = abs a
                    len = length v
                    nxa = abn*aba in
    if nxa + n >= len || n > len
    then []
    else take n $ drop nxa v

vec2At :: Num t => Int -> [t] -> (t, t)
vec2At n v = tuple $ vecNAt 2 (abs n) v
    where tuple [x,y] = (x,y)
          tuple _     = (0,0)

vec3At :: Num t => Int -> [t] -> (t, t, t)
vec3At n v = tuple $ vecNAt 3 (abs n) v
    where tuple [x,y,z] = (x,y,z)
          tuple _       = (0,0,0)

vec4At :: Num t => Int -> [t] -> (t, t, t, t)
vec4At n v = tuple $ vecNAt 4 (abs n) v
    where tuple [x,y,z,w] = (x,y,z,w)
          tuple _         = (0,0,0,0)

