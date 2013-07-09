--{-# LANGUAGE TemplateHaskell #-}
module Graphics.Scene where

import Geometry
import Graphics.Resource
import Graphics.Shaders
import Graphics.Vbo
import Graphics.Rendering.OpenGL.Raw
import Data.Maybe
import Control.Lens
import qualified Data.Map as M
import Graphics.Rendering.OpenGL hiding ( Matrix, Position, Size )

type Position = (GLfloat, GLfloat)
type Size     = (GLfloat, GLfloat)

data DisplayObject = TextChar Char Position Size
                   | TextString String Position Size
                   | ColoredTri Position Size
                   deriving (Show, Eq)

data SceneGraph = SceneNode (Transform3d GLfloat) DisplayObject
                | SceneGraph (Transform3d GLfloat) [SceneGraph]
                | SceneRoot Int Int SceneGraph deriving (Show, Eq)


renderSceneGraphAt :: Matrix GLfloat -> ResourceStore -> SceneGraph -> IO ()
renderSceneGraphAt mat rez (SceneRoot w h sg) = do
    let pgms  = rez ^. programMap . to M.elems
        orth  = orthoMatrix 0 (fromIntegral w) 0 (fromIntegral h) 0 1 :: Matrix GLfloat
        upd   = updateUniform $ UpdateMat4 "projection" orth
    mapM_ upd pgms
    renderSceneGraphAt mat rez sg

renderSceneGraphAt mat rez (SceneGraph tfrm chldn) =
    let mat' = applyTransformation tfrm mat
        rndr = renderSceneGraphAt mat' rez
    in mapM_ rndr chldn

renderSceneGraphAt mat rez (SceneNode tfrm dObj) =
    let mat' = applyTransformation tfrm mat
    in renderDisplayObject mat' rez dObj


renderDisplayObject :: Matrix GLfloat -> ResourceStore -> DisplayObject -> IO ()
renderDisplayObject mat rez (ColoredTri (x,y) (w,h)) = do
    let tfrm = (Rotation 0 0 0, Scale w h 1, Translation x y 0)
        mat' = applyTransformation tfrm mat
        mVbo = M.lookup "tri" $ rez ^. vboMap
        mPgm = M.lookup "color" $ rez ^. programMap
        vbo  = fromJust mVbo
        shdr = fromJust mPgm
        ufrm = UpdateMat4 "modelview" mat'
    if isJust mVbo &&
        isJust mPgm
      then do True <- updateUniform ufrm shdr
              bindInterleavedVbo vbo
              drawArrays Triangles 0 6
      else do putStrLn "Could not render Tri."
              print mVbo
              print mPgm

--renderDisplayObject mat rez (TextChar ch (x,y) (w,h)) = do
--    let tfrm = (Rotation 0 0 0, Scale w h 1, Translation x y 0)
--        mat' = applyTransformation tfrm mat
--        ndx  = fromIntegral $ fromEnum ch - 33
--        names = ["projection","modelview","sampler","color"]
--        upMat l = glUniformMatrix4fv l 1 1
--        upSam l _ = glUniform1i l 0
--        upCol l = glUniform4fv l 1
--        ups = [upMat,upMat,upSam,upCol]
--        arrs = [concat (identityN 4 :: Matrix GLfloat), concat mat', [1.0,0.0,1.0,1.0], []]
--        mTex = M.lookup "font" $ rez ^. textureMap
--        mVbo = M.lookup "font" $ rez ^. vboMap
--        mPgm = M.lookup "font" $ rez ^. programMap
--        tex  = fromJust mTex
--        vbo  = fromJust mVbo
--        spgm = fromJust mPgm
--        pgm  = fromJust $ _program spgm
--
--    if isJust mTex &&
--        isJust mVbo &&
--         isJust mPgm
--      then do currentProgram $= Just pgm
--              updateUniforms names ups arrs
--              texture Texture2D $= Enabled
--              activeTexture     $= TextureUnit 0
--              textureBinding Texture2D $= Just tex
--              bindInterleavedVbo vbo
--              drawArrays TriangleFan (4*ndx) 4
--      else do putStrLn "Could not render Text."
--              print mTex
--              print mVbo
--              print mPgm

renderDisplayObject _ _ _ = putStrLn "Cannot render unknown DisplayObject"

