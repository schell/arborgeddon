module Graphics.Scene where

import Geometry
import Graphics.Resource
import Graphics.Shaders
import Graphics.Vbo
import Graphics.Scene.Types
import Graphics.Rendering.OpenGL.Raw
import Data.Maybe
import Data.Monoid
import Control.Lens
import Control.Monad
import Control.Monad.State
import qualified Data.Map as M
import Graphics.Rendering.OpenGL hiding ( Matrix, translate )


{- Mutating the Scene -}

addChild :: SceneGraph
         -> SceneRoot
         -> SceneRoot
addChild c (SceneRoot i w h _) = SceneRoot i w h $ Just c

addGraph :: SceneGraph  -- The child to add
         -> SceneGraph  -- The parent to add it to
         -> SceneGraph  -- The new parent after the addition
addGraph c p@(SceneGraph _ ch) = execState (sGraphChildren .= ch ++ [Left c]) p

addNode :: SceneNode  -- The child node to add
        -> SceneGraph -- The parent graph
        -> SceneGraph -- The new graph
addNode c p@(SceneGraph _ ch) = execState (sGraphChildren .= ch ++ [Right c]) p

add :: Either SceneGraph SceneNode
    -> SceneGraph
    -> SceneGraph
add (Left g)  = addGraph g
add (Right n) = addNode n

removeGraph :: SceneGraph -- The child to remove
            -> SceneGraph -- The parent to remove it from
            -> SceneGraph -- The new parent after the removal
removeGraph c g = flip execState g $ do
    ch <- use sGraphChildren
    sGraphChildren .= filter (/= Left c) ch

removeNode :: SceneNode -- The child to remove
           -> SceneGraph -- The parent to remove it from
           -> SceneGraph -- The new parent after the removal
removeNode c g = flip execState g $ do
    ch <- use sGraphChildren
    sGraphChildren .= filter (/= Right c) ch

remove :: Either SceneGraph SceneNode -- The child to remove
          -> SceneGraph -- The parent to remove it from
          -> SceneGraph -- The new parent after the removal
remove (Left g)  = removeGraph g
remove (Right n) = removeNode n

{- Rendering the Scene -}

renderRootAt :: Matrix GLfloat -> ResourceStore -> SceneRoot -> IO ()
renderRootAt mat rez (SceneRoot _ w h mSg) = when (isJust mSg) $ do
    let sg    = fromJust mSg
        pgms  = rez ^. programMap . to M.elems
        orth  = orthoMatrix 0 (fromIntegral w) 0 (fromIntegral h) 0 1 :: Matrix GLfloat
        upd   = flip updateUniform $ UpdateMat4 "projection" orth
    viewport $= (Position 0 0, Size (fromIntegral w) (fromIntegral h))
    mapM_ upd pgms
    renderGraphAt mat rez sg

renderGraphAt :: Matrix GLfloat -> ResourceStore -> SceneGraph -> IO ()
renderGraphAt mat rez (SceneGraph (NMetaData _ tfrm) chldn) =
    let mat' = applyTransformation tfrm mat
        rndr = renderGraphOrNodeAt mat' rez
    in mapM_ rndr chldn

renderNodeAt :: Matrix GLfloat -> ResourceStore -> SceneNode -> IO ()
renderNodeAt mat rez (SceneNode (NMetaData _ tfrm) dObj) =
    let mat' = applyTransformation tfrm mat
    in renderDisplayObject mat' rez dObj

renderGraphOrNodeAt :: Matrix GLfloat -> ResourceStore -> Either SceneGraph SceneNode -> IO ()
renderGraphOrNodeAt mat rez (Left g) = renderGraphAt mat rez g
renderGraphOrNodeAt mat rez (Right n) = renderNodeAt mat rez n

renderDisplayObject :: Matrix GLfloat -> ResourceStore -> DisplayObject -> IO ()
renderDisplayObject mat rez ColoredTri = do
    let mVbo = M.lookup "tri" $ rez ^. vboMap
        mPgm = M.lookup "color" $ rez ^. programMap
        vbo  = fromJust mVbo
        shdr = fromJust mPgm
        ufrm = UpdateMat4 "modelview" mat
    if isJust mVbo &&
        isJust mPgm
      then do True <- updateUniform shdr ufrm
              bindInterleavedVbo vbo
              drawArrays Triangles 0 6
      else do putStrLn "Could not render Tri."
              print mVbo
              print mPgm

renderDisplayObject mat rez (TextChar ch) = do
    let ndx  = fromIntegral $ fromEnum ch - 33
        mUfrm = UpdateMat4 "modelview" mat
        cUfrm = UpdateVec4 "color" (1.0,1.0,1.0,1.0)
        sUfrm = UpdateSampler "sampler" 0
        ufrms = [mUfrm,cUfrm,sUfrm]
        mTex = M.lookup "font" $ rez ^. textureMap
        mVbo = M.lookup "font" $ rez ^. vboMap
        mPgm = M.lookup "font" $ rez ^. programMap
        tex  = fromJust mTex
        vbo  = fromJust mVbo
        shdr = fromJust mPgm

    if isJust mTex &&
        isJust mVbo &&
         isJust mPgm
      then do mapM_ (updateUniform shdr) ufrms
              texture Texture2D $= Enabled
              activeTexture     $= TextureUnit 0
              textureBinding Texture2D $= Just tex
              bindInterleavedVbo vbo
              drawArrays TriangleFan (4*ndx) 4
      else do putStrLn "Could not render Text."
              print mTex
              print mVbo
              print mPgm

renderDisplayObject mat rez (TextString s) = zipWithM_ renderDisplayObject' s mats'
    where renderDisplayObject' ch mat' = renderDisplayObject mat' rez $ TextChar ch
          spacing = 0.6
          (_,_,mats') = foldl accf (0,0,[]) s
          makeMat x y = applyTransformation (translate x y 0 mempty) mat
          accf (x,y,mats) ch = if ch == '\n'
                                  then (0,y+spacing, mats ++ [makeMat x y])
                                  else (x+spacing,y, mats ++ [makeMat x y])

