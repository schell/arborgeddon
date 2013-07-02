{-# LANGUAGE TemplateHaskell #-}
module Graphics.Scene where

import Graphics.Vbo
import Graphics.Texture
import Graphics.Util
import Geometry.Types
import Geometry.Matrix
import Geometry.MatrixTransformations
import Data.Monoid
import Graphics.Rendering.OpenGL.Raw
import Graphics.Rendering.OpenGL.GL.Shaders.Program
import Foreign.Ptr              ( Ptr )
import Foreign.Marshal.Array    ( withArray )
import Foreign.C.String         ( withCString )
import Graphics.UI.GLFW         ( swapBuffers )
import qualified Data.Map as M
import Control.Lens              hiding ( transform )
import Graphics.Rendering.OpenGL hiding ( Matrix )

data SceneGraph = SceneGraph { _graphId   :: Int
                             , _transform :: Transform3d GLfloat
                             , _nodes     :: [SceneGraph]
                             , _toScreen  :: Matrix GLfloat -> IO ()
                             }
makeLenses ''SceneGraph

renderSceneGraph :: Matrix GLfloat -> SceneGraph -> IO ()
renderSceneGraph m s =
    let m' = applyTransformation (s^.transform) m
    in do
        (s^.toScreen) m'
        mapM_ (renderSceneGraph m') $ s^.nodes

emptySceneGraph :: SceneGraph
emptySceneGraph = SceneGraph 0 mempty [] (\_ -> return ())

type VboMap = M.Map String (Maybe InterleavedVbo)

data Scene = Scene { _sceneId  :: Int
                   , _graph    :: SceneGraph
                   , _textures :: TextureMap
                   , _vbos     :: VboMap
                   }
makeLenses ''Scene

renderScene :: (Int, Int) -> Scene -> IO ()
renderScene (w,h) scene = do
    let mId   = identityN 4 :: Matrix GLfloat
        p     = orthoMatrix 0 (fromIntegral w) 0 (fromIntegral h) 0 1 :: Matrix GLfloat
        ups   = [matUpdate, matUpdate, samUpdate]
        names = ["projection","modelview","sampler"]
        arrs  = map concat [p, mId, []]
        matUpdate loc   = glUniformMatrix4fv loc 1 1
        samUpdate loc _ = glUniform1i loc 0
    -- Clear the screen and the depth buffer.
    clear [ColorBuffer, DepthBuffer]
    -- Set the viewport.
    viewport $= (Position 0 0, Size (fromIntegral w) (fromIntegral h))
    -- Update the matrix uniforms.
    updateUniforms names ups arrs
    -- Render our scene.
    renderSceneGraph mId $ scene^.graph
    swapBuffers

updateUniforms :: [String]                        -- ^ A list of uniform names.
               -> [GLint -> Ptr GLfloat -> IO ()] -- ^ A list of update functions.
               -> [[GLfloat]]                     -- ^ A list of uniform arrays.
               -> IO ()
updateUniforms names updates datas = do
    p    <- getCurrentProgram
    locs <- mapM (\name -> do
        loc <- withCString name $ \ptr ->
            glGetUniformLocation (programID p) ptr
        printError
        return loc) names
    sequence_ $ zipWith3 (\upd arr loc -> do
        withArray arr $ upd loc
        printError) updates datas locs

emptyScene :: Scene
emptyScene = Scene 0 emptySceneGraph mempty mempty

