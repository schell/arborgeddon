{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Geometry.Transformation where

import Geometry.Vector
import Data.SafeCopy
import Data.Monoid
import Control.Applicative
import Geometry.Matrix
import Graphics.Rendering.OpenGL ( GLfloat )

class Transformable a where
    rotate    :: GLfloat -> GLfloat -> GLfloat -> a -> a
    scale     :: GLfloat -> GLfloat -> GLfloat -> a -> a
    translate :: GLfloat -> GLfloat -> GLfloat -> a -> a

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

{- Transforming other things -}

applyTransformation :: (Num a, RealFrac a, Floating a, Show a) => Transform3d a -> Matrix a -> Matrix a
applyTransformation t = rotate3d rx ry rz . scale3d sx sy sz . translate3d tx ty tz
    where (Rotation rx ry rz, Scale sx sy sz, Translation tx ty tz) = t

transformToMatrix :: (Num a, RealFrac a, Floating a, Show a) => Transform3d a -> Matrix a
transformToMatrix = (`applyTransformation` identityN 4)

{- Transforming Matrices -}
scale3d :: (Num a, RealFrac a, Floating a, Show a) => a -> a -> a -> Matrix a -> Matrix a
scale3d x y z m = multiply m s
    where s = scaleMatrix3d x y z

rotate3d :: (Num a, RealFrac a, Floating a, Show a) => a -> a -> a -> Matrix a -> Matrix a
rotate3d x y z m = multiply m r
    where r = rotationMatrix3d x y z

translate3d :: (Num a, RealFrac a, Floating a, Show a) => a -> a -> a -> Matrix a -> Matrix a
translate3d x y z m = multiply m t
    where t = translationMatrix3d x y z

transformPoint2d :: (Num a, Show a) => Matrix a -> Vec2 a -> Vec2 a
transformPoint2d m (x,y) = (x',y')
    where (x',y',_) = transformPoint3d m (x,y,0)

transformPoint3d :: (Num a, Show a) => Matrix a -> Vec3 a -> Vec3 a
transformPoint3d m (x,y,z) = (x',y',z')
    where [[x', y', z',_]] = multiply m [[x],[y],[z],[1]]

transformVector3 :: (Num a, Show a) => Matrix a -> Vector a -> Vector a
transformVector3 m v = makeVector $ map (transformPoint3d m) $ makeVec3List v
    where makeVec3List = map (\[x,y,z] -> (x,y,z)) . groupByRowsOf 3
          makeVector   = concatMap (\(x,y,z) -> [x,y,z])
