{-# LANGUAGE TemplateHaskell #-}
module Graphics.Scene.Types where

import Geometry
import Control.Lens
import Data.Monoid
import Graphics.Rendering.OpenGL ( GLfloat )

data DisplayObject = TextChar Char
                   | TextString String
                   | ColoredTri
                   deriving (Show, Eq)

data NodeMetaData = NMetaData { _nodeId        :: Int
                              , _nodeTransform :: Transform3d GLfloat
                              } deriving (Show, Eq)
makeLenses ''NodeMetaData

instance Monoid NodeMetaData where
    mempty = NMetaData { _nodeId = 0 
                       , _nodeTransform = mempty
                       }
    mappend a b = NMetaData { _nodeId = _nodeId b 
                            , _nodeTransform = _nodeTransform a `mappend` _nodeTransform b
                            }


data SceneNode = SceneNode { _sNodeData   :: NodeMetaData
                           , _sNodeObject :: DisplayObject
                           } deriving (Eq, Show)
makeLenses ''SceneNode

data SceneGraph = SceneGraph { _sGraphData      :: NodeMetaData
                             , _sGraphChildren  :: [Either SceneGraph SceneNode]
                             } deriving (Eq, Show)
makeLenses ''SceneGraph

data SceneRoot = SceneRoot  { _sRootNextId     :: Int
                            , _sRootWidth      :: Int
                            , _sRootHeight     :: Int
                            , _sRootGraph      :: Maybe SceneGraph
                            }
                            deriving (Show, Eq)
makeLenses ''SceneRoot

