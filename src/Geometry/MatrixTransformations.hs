module Geometry.MatrixTransformations where

import Geometry.Matrix
import Geometry.Types

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
