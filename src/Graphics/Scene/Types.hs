{-# LANGUAGE TemplateHaskell #-}
module Graphics.Scene.Types where

import Geometry
import Graphics.TypeClasses

import Control.Lens
import Graphics.Rendering.OpenGL ( GLfloat )
import qualified Data.IntMap as M

data DisplayObject = TextChar Char
                   | TextString String
                   | ColoredTri
                   deriving (Show, Eq)

data Scene a = Scene  { _sceneWidth      :: Int
                      , _sceneHeight     :: Int
                      , _sceneNodes      :: [Node a]
                      , _sceneTransforms :: PathMap
                      } deriving (Show, Eq)

data Node a = Node { _nodePath      :: [Int]
                   , _nodeObject    :: a
                   , _nodeTransform :: Transform3d GLfloat
                   } deriving (Show, Eq)

type PathMap = M.IntMap (Transform3d GLfloat)

makeLenses ''Node
makeLenses ''Scene

{- Class instances -}
instance Transformable (Node a) where
    scale x y z n     = nodeTransform .~ scale x y z (n ^. nodeTransform) $ n
    rotate x y z n    = nodeTransform .~ rotate x y z (n ^. nodeTransform) $ n
    translate x y z n = nodeTransform .~ translate x y z (n ^. nodeTransform) $ n

