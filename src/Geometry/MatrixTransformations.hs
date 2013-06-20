module Geometry.MatrixTransformations where

import Geometry.Matrix
import Geometry.Types

applyTransformation :: (Num a, RealFrac a, Floating a) => Transform3d a -> Matrix a -> Matrix a
applyTransformation t = rotate3d rx ry rz . scale3d sx sy sz . translate3d tx ty tz
    where (Rotation rx ry rz, Scale sx sy sz, Translation tx ty tz) = t

{- Transforming Matrices -}
scale3d :: (Num a, RealFrac a, Floating a) => a -> a -> a -> Matrix a -> Matrix a
scale3d x y z m = multiply m s
    where s = scaleMatrix3d x y z

rotate3d :: (Num a, RealFrac a, Floating a) => a -> a -> a -> Matrix a -> Matrix a
rotate3d x y z m = multiply m r
    where r = rotationMatrix3d x y z

translate3d :: (Num a, RealFrac a, Floating a) => a -> a -> a -> Matrix a -> Matrix a
translate3d x y z m = multiply m t
    where t = translationMatrix3d x y z

