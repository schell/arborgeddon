module Arbor.Data.Vector (
    Vector,
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

import Arbor.Data.Types

-- | Computes the magnitude.
magnitude :: Vector -> Float
magnitude = sqrt . sum . map (**2)

-- | Computes the unit vector.
normalize :: Vector -> Vector
normalize vec = map (/mag) vec
    where mag = magnitude vec

-- | Computes the unit vector.
unitize :: Vector -> Vector
unitize = normalize

-- | Adds two vectors.
add :: Vector -> Vector -> Vector
add = zipWith (+)

-- | Subtracts two vectors.
subtract :: Vector -> Vector -> Vector
subtract = zipWith (-)

-- | Returns the a'th vector of n components in the given vector.
vecNAt :: Int -> Int -> Vector -> Vector
vecNAt n a v  = let abn = abs n
                    aba = abs a
                    len = length v
                    nxa = abn*aba in
    if nxa + n >= len || n > len
    then []
    else take n $ drop nxa v

vec2At :: Int -> Vector -> Vec2
vec2At n v = tuple $ vecNAt 2 (abs n) v
    where tuple [x,y] = (x,y)
          tuple _     = (0,0)

vec3At :: Int -> Vector -> Vec3
vec3At n v = tuple $ vecNAt 3 (abs n) v
    where tuple [x,y,z] = (x,y,z)
          tuple _       = (0,0,0)

vec4At :: Int -> Vector -> Vec4
vec4At n v = tuple $ vecNAt 4 (abs n) v
    where tuple [x,y,z,w] = (x,y,z,w)
          tuple _         = (0,0,0,0)

