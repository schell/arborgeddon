{-# LANGUAGE TemplateHaskell #-}
module Graphics.Scene.Types where

import Geometry
import Graphics.TypeClasses

import Control.Lens
import Data.Monoid
import Graphics.Rendering.OpenGL ( GLfloat )
import qualified Data.IntMap as IM

data DisplayObject = TextChar Char
                   | TextString String
                   | ColoredTri
                   | ColoredSquare
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
    scale x y z      = nodeTransform %~ scale x y z
    rotate x y z     = nodeTransform %~ rotate x y z
    translate x y z  = nodeTransform %~ translate x y z

instance Transformable NodeContainer where
    scale x y z (NodeContainer (i,pm))     = NodeContainer (i, IM.adjust (scale x y z) i pm)
    rotate x y z (NodeContainer (i,pm))    = NodeContainer (i, IM.adjust (rotate x y z) i pm)
    translate x y z (NodeContainer (i,pm)) = NodeContainer (i, IM.adjust (translate x y z) i pm)

{- For building the scene -}
data SceneGraph a = SceneGraph { _sgTransform :: Transform3d GLfloat
                               , _sgNodes     :: [SceneGraph a]
                               }
                  | SceneNode  { _snTransform :: Transform3d GLfloat
                               , _snObj       :: a
                               } deriving (Eq)
makeLenses ''SceneGraph

instance Show a => Show (SceneGraph a) where
    show = showSceneGraph

instance Transformable (SceneGraph a) where
    scale x y z s@SceneGraph{}     = sgTransform %~ scale x y z $ s
    scale x y z s@SceneNode{}      = snTransform %~ scale x y z $ s
    rotate x y z s@SceneGraph{}    = sgTransform %~ rotate x y z $ s
    rotate x y z s@SceneNode{}     = snTransform %~ rotate x y z $ s
    translate x y z s@SceneGraph{} = sgTransform %~ translate x y z $ s
    translate x y z s@SceneNode{}  = snTransform %~ translate x y z $ s

instance Monoid (SceneGraph a) where
    mempty = SceneGraph mempty []
    mappend a b = SceneGraph mempty [a,b]

tri :: GLfloat -> GLfloat -> GLfloat -> SceneGraph DisplayObject
tri x y z = scale x y z $ SceneNode mempty ColoredTri

tri16x16 :: SceneGraph DisplayObject
tri16x16 = tri 16 16 1

triline :: SceneGraph DisplayObject
triline = foldl addTri mempty [0..10]
    where addTri r i = r <+ translate (i*8) 0 0 tri16x16


(<^>) :: SceneGraph a -> SceneGraph a -> SceneGraph a
(<^>) = mappend

(+>) :: SceneGraph a -> SceneGraph a -> SceneGraph a
(+>) s1 s2@SceneGraph{} = sgNodes %~ (++ [s1]) $ s2
(+>) s1 s2@SceneNode{} =
    -- Since the programmer wants to add some graph to a node
    -- and nodes can't hold anything, we create a new graph
    -- at the node's transform, set the node's transform to
    -- the id and then add the other graph after the node.
    let tfrm = s2 ^. snTransform
        s2'  = SceneNode mempty $ _snObj s2
        g    = sgTransform .~ tfrm $ sgNodes .~ [s2',s1] $ mempty
    in g

(<+) :: SceneGraph a -> SceneGraph a -> SceneGraph a
(<+) = flip (+>)

triBox :: SceneGraph DisplayObject
triBox = foldl (<+) mempty [left,top,right,bottom]
    where top    = triline
          bottom = translate 0 100 0 top
          left   = rotate 0 0 (pi/2) top
          right  = translate 100 0 0 left

sceneGraphToScene :: SceneGraph a -> Scene a
sceneGraphToScene s = sceneTransforms .~ trfrms $ sceneNodes .~ nodes $ scene
    where scene  = Scene 0 0 [] mempty
          trfrms = sceneGraphToPathMap s
          nodes  = sceneGraphToNodes s

sceneGraphToPathMap :: SceneGraph a -> PathMap
sceneGraphToPathMap SceneNode{} = mempty
sceneGraphToPathMap g@SceneGraph{} = foldl sg2pm (incPM mempty g) (g ^. sgNodes)
    where sg2pm pm SceneNode{}     = pm
          sg2pm pm g'@SceneGraph{} = foldl sg2pm (incPM pm g') (g' ^. sgNodes)
          incPM pm g'              = IM.insert (IM.size pm) (g' ^. sgTransform) pm

sceneGraphToNodes :: SceneGraph a -> [Node a]
sceneGraphToNodes n@SceneNode{}  = [Node [] (_snObj n) (_snTransform n)]
sceneGraphToNodes s@SceneGraph{} = ns'
    where (_,_,ns') = sg2n (0,[],[]) s
          sg2n (i,p,ns) n@SceneNode{}  = (i, p, ns ++ [Node p (_snObj n) (_snTransform n)])
          sg2n (i,p,ns) s'@SceneGraph{} = foldl sg2n (i+1, p ++ [i], ns) $ _sgNodes s'

showSceneGraph :: Show a => SceneGraph a -> String
showSceneGraph = showSG 0
    where showSG i s@SceneNode{}  = unwords [prefix i, show (_snObj s), show (_snTransform s)]
          showSG i s@SceneGraph{} = " " ++ prefix i ++ show (_sgTransform s) ++ concatMap (\s' -> '\n' :showSG (i+1) s') (_sgNodes s)
          prefix i = concat $ replicate i "    "
