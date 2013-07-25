module Game.DisplayObject.TestObject where

import Graphics
import Geometry
import Data.Monoid
import Data.Maybe
import Control.Lens
import Control.Monad
import qualified Data.Map as M
import Graphics.Rendering.OpenGL hiding ( Matrix, translate, scale, rotate )


data TestObject = TextChar Char
                | TextString String
                | ColoredTri
                | ColoredSquare
                deriving (Show, Eq)

instance SceneData TestObject where
    renderData = renderTestObject

tri :: GLfloat -> GLfloat -> GLfloat -> SceneGraph TestObject
tri x y z = scale x y z $ SceneNode mempty ColoredTri

tri16x16 :: SceneGraph TestObject
tri16x16 = tri 16 16 1

triline :: SceneGraph TestObject
triline = foldl addTri mempty [0..10]
    where addTri r i = r <+ translate (i*8) 0 0 tri16x16

triBox :: SceneGraph TestObject
triBox = foldl (<+) mempty [left,top,right,bottom]
    where top    = triline
          bottom = translate 0 100 0 top
          left   = rotate 0 0 (pi/2) top
          right  = translate 100 0 0 left


{- Rendering -}

renderTestObject :: Matrix GLfloat -> ResourceStore -> TestObject -> IO ()
renderTestObject mat rez ColoredTri = do
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

renderTestObject mat rez ColoredSquare = do
    let mVbo = M.lookup "square" $ rez ^. vboMap
        mPgm = M.lookup "color" $ rez ^. programMap
        vbo  = fromJust mVbo
        shdr = fromJust mPgm
        ufrm = UpdateMat4 "modelview" mat
    if isJust mVbo &&
        isJust mPgm
      then do True <- updateUniform shdr ufrm
              bindInterleavedVbo vbo
              drawArrays Triangles 0 12
      else do putStrLn "Could not render Square."
              print mVbo
              print mPgm

renderTestObject mat rez (TextChar ch) = do
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

renderTestObject mat rez (TextString s) = zipWithM_ renderTestObject' s mats'
    where renderTestObject' ch mat' = renderTestObject mat' rez $ TextChar ch
          spacing = 0.6
          (_,_,mats') = foldl accf (0,0,[]) s
          makeMat x y = applyTransformation (translate x y 0 mempty) mat
          accf (x,y,mats) ch = if ch == '\n'
                                  then (0,y+spacing, mats ++ [makeMat x y])
                                  else (x+spacing,y, mats ++ [makeMat x y])

