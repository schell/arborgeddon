{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Geometry.Types where

import Data.SafeCopy
import Data.Monoid
import Control.Applicative
import Graphics.TypeClasses
import Graphics.Rendering.OpenGL ( GLfloat )

{- Rectangle -}
-- | A rect at (x,y) with (width,height).
data Rectangle a = Rectangle (Vec2 a) (Vec2 a) deriving (Show, Eq)

toTriangleRect :: (Num a) => Rectangle a -> [a]
toTriangleRect (Rectangle (x,y) (w,h)) = concat [tl, bl, br, br, tl, tr]
    where tl = [x,y]
          tr = [x+w,y]
          bl = [x,y+h]
          br = [x+w,y+h]

toTriangles :: (Num a) => [Rectangle a] -> [a]
toTriangles = concatMap toTriangleRect

{- The Matrix -}
type Matrix a = [Vector a]

{- Vector Types -}
type Vector a = [a]

type Vec2 a = (a, a)
type Vec3 a = (a, a, a)
type Vec4 a = (a, a, a, a)

{- Transformations -}
type Transform3d a = (Rotation3d a, Scale3d a, Translation3d a)

instance Transformable (Transform3d GLfloat) where
    scale x' y' z' (r,Scale x y z,t) = (r,Scale (x*x') (y*y') (z*z'),t)
    translate x' y' z' (r,s,Translation x y z) = (r,s,Translation (x+x') (y+y') (z+z'))
    rotate x' y' z' (Rotation x y z,s,t) = (Rotation (x+x') (y+y') (z+z'),s,t)

data Rotation3d a = Rotation a a a deriving (Show, Eq)

instance Num a => Monoid (Rotation3d a) where
    mempty = Rotation 0 0 0
    mappend (Rotation x1 y1 z1) (Rotation x2 y2 z2) = Rotation (x1+x2) (y1+y2) (z1+z2)
instance (SafeCopy a) => SafeCopy (Rotation3d a) where
    putCopy (Rotation x y z) = contain $ safePut [x,y,z]
    getCopy = contain $ (\[x,y,z] -> Rotation x y z) <$> safeGet


data Translation3d a = Translation a a a deriving (Show, Eq)

instance Num a => Monoid (Translation3d a) where
    mempty = Translation 0 0 0
    mappend (Translation x1 y1 z1) (Translation x2 y2 z2) = Translation (x1+x2) (y1+y2) (z1+z2)
instance (SafeCopy a) => SafeCopy (Translation3d a) where
    putCopy (Translation x y z) = contain $ safePut [x,y,z]
    getCopy = contain $ (\[x,y,z] -> Translation x y z) <$> safeGet


data Scale3d a = Scale a a a deriving (Show, Eq)

instance Num a => Monoid (Scale3d a) where
    mempty = Scale 1 1 1
    mappend (Scale x1 y1 z1) (Scale x2 y2 z2) = Scale (x1*x2) (y1*y2) (z1*z2)
instance (SafeCopy a) => SafeCopy (Scale3d a) where
    putCopy (Scale x y z) = contain $ safePut [x,y,z]
    getCopy = contain $ (\[x,y,z] -> Scale x y z) <$> safeGet

