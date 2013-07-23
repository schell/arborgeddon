module Graphics.Scene where

import Geometry
import Graphics.Resource
import Graphics.Shaders
import Graphics.Vbo
import Graphics.Scene.Types
import Graphics.Rendering.OpenGL.Raw
import Graphics.TypeClasses
import Data.Maybe
import Data.Monoid
import Debug.Trace
import Control.Lens
import Control.Monad
import Control.Monad.State
import qualified Data.Map    as M
import qualified Data.IntMap as IM
import Graphics.Rendering.OpenGL hiding ( Matrix, translate )


{- Mutating the Scene -}

setPath :: [Int] -> Node a -> Node a
setPath p n = nodePath .~ p $ n

addNode :: Node a -> Scene a -> Scene a
addNode n = execState $ do
    ns <- use sceneNodes
    sceneNodes .= ns ++ [n]

initPath :: Int -> PathMap -> PathMap
initPath c = IM.insert c $ trace (show mat) mat
    where mat = mempty

{- Rendering the Scene -}

renderScene :: ResourceStore -> Scene DisplayObject -> IO ()
renderScene rez (Scene w h ns ts) = unless (null ns) $ do
    let pgms  = rez ^. programMap . to M.elems
        orth  = orthoMatrix 0 (fromIntegral w) 0 (fromIntegral h) 0 1 :: Matrix GLfloat
        upd   = flip updateUniform $ UpdateMat4 "projection" orth
    viewport $= (Position 0 0, Size (fromIntegral w) (fromIntegral h))
    mapM_ upd pgms
    mapM_ (renderNode rez ts) ns

renderNode :: ResourceStore -> PathMap -> Node DisplayObject -> IO ()
renderNode rez pm n =
    let mTfrm = fmap mconcat $ mapM (`IM.lookup` pm) $ n ^. nodePath
        mat   = applyTransformation (n ^. nodeTransform) $ identityN 4
        mMat' = fmap (`applyTransformation` mat) mTfrm
    in unless (isNothing mMat') $ renderDisplayObject (fromJust mMat') rez $ n ^. nodeObject

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

