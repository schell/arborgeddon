module Graphics.Scene where

import Geometry
import Graphics.Resource
import Graphics.Shaders
import Graphics.Scene.Types
import Graphics.Rendering.OpenGL.Raw
import Data.Maybe
import Control.Lens
import Control.Monad
import qualified Data.Map    as M
import qualified Data.IntMap as IM
import Graphics.Rendering.OpenGL hiding ( Matrix, translate )


{- Rendering the Scene -}

renderScene :: SceneData a => ResourceStore -> Scene a -> IO ()
renderScene rez (Scene w h ns ts) = unless (null ns) $ do
    let pgms  = rez ^. programMap . to M.elems
        orth  = orthoMatrix 0 (fromIntegral w) 0 (fromIntegral h) 0 1 :: Matrix GLfloat
        upd   = flip updateUniform $ UpdateMat4 "projection" orth
    viewport $= (Position 0 0, Size (fromIntegral w) (fromIntegral h))
    mapM_ upd pgms
    mapM_ (renderNode rez ts) ns

renderNode :: SceneData a => ResourceStore -> PathMap -> Node a -> IO ()
renderNode rez pm n = renderData mat rez $ n ^. nodeObject
    where mat = getCompoundMatrix pm n

getCompoundMatrix :: SceneData a => PathMap -> Node a -> Matrix GLfloat
getCompoundMatrix pm n =
    let mTfrms = mapM (`IM.lookup` pm) $ n ^. nodePath
        mat    = transformToMatrix $ n ^. nodeTransform
        mMat'  = fmap fldMat mTfrms
        fldMat = foldr (\t m -> transformToMatrix t `multiply` m) mat
    in fromMaybe mat mMat'


