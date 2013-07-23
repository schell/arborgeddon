{-# LANGUAGE TemplateHaskell #-}
module Graphics.Scene.Types where

import Geometry
import Graphics.TypeClasses

import Control.Lens
import Graphics.Rendering.OpenGL ( GLfloat )
import qualified Data.IntMap as IM

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

type PathMap = IM.IntMap (Transform3d GLfloat)

data NodeContainer = NodeContainer (Int, PathMap) deriving (Show,Eq)

makeLenses ''Node
makeLenses ''Scene

{- Class instances -}
instance Transformable (Node a) where
    scale x y z n     = nodeTransform .~ scale x y z (n ^. nodeTransform) $ n
    rotate x y z n    = nodeTransform .~ rotate x y z (n ^. nodeTransform) $ n
    translate x y z n = nodeTransform .~ translate x y z (n ^. nodeTransform) $ n

instance Transformable NodeContainer where
    scale x y z (NodeContainer (i,pm))     = NodeContainer (i, IM.adjust (scale x y z) i pm)
    rotate x y z (NodeContainer (i,pm))    = NodeContainer (i, IM.adjust (rotate x y z) i pm)
    translate x y z (NodeContainer (i,pm)) = NodeContainer (i, IM.adjust (translate x y z) i pm)

