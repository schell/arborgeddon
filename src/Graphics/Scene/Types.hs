{-# LANGUAGE TemplateHaskell #-}
module Graphics.Scene.Types where

import Geometry
import Control.Lens
--import Data.Monoid
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

