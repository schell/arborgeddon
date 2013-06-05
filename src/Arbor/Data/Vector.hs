module Arbor.Data.Vector where

type Vector = [Float]

-- | Computes the magnitude.
magnitude :: Vector -> Float
magnitude = sqrt . sum . map (**2)

-- | Computes the unit vector.
normalize :: Vector -> Vector
normalize vec = map (/mag) vec
    where mag = magnitude vec

unitVector :: Vector -> Vector
unitVector = normalize
