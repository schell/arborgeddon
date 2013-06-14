module Geometry.Transformations where

import Data.Monoid
import Data.SafeCopy
import Control.Applicative

type Transform3d a = (Rotation3d a, Scale3d a, Translation3d a)


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
    mappend (Scale x1 y1 z1) (Scale x2 y2 z2) = Scale (x1+x2) (y1+y2) (z1+z2)
instance (SafeCopy a) => SafeCopy (Scale3d a) where
    putCopy (Scale x y z) = contain $ safePut [x,y,z]
    getCopy = contain $ (\[x,y,z] -> Scale x y z) <$> safeGet



