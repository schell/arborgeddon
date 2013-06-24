{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Graphics.Renderable where

import Geometry
import Game.Types
import Graphics.Util
import Graphics.Vbo
import Graphics.Rendering.OpenGL.Raw
import Graphics.Rendering.OpenGL.GL.Shaders.Program

import Graphics.Rendering.OpenGL hiding ( Matrix )
import Control.Lens              hiding ( transform )

import Foreign.Marshal.Array         ( withArray )
import Foreign.C.String              ( withCString )
import Foreign.Ptr                   ( Ptr )

import Graphics.UI.GLFW as GLFW

{- Rendering Typeclass -}
class Renderable a where
    render :: a -> IO ()

instance Renderable Game where
    render game = do
        -- Clear the screen and the depth buffer.
        clear [ColorBuffer, DepthBuffer]
        -- Update the matrix uniforms.
        let t          = game^.scene.transform
            (tex,ivbo) = game^.renderSrcs
            mv         = applyTransformation t $ identityN 4
            p          = identityN 4
        -- Texture
        activeTexture     $= TextureUnit 0
        textureBinding Texture2D $= Just tex
        printError

        updateMatrixUniforms p mv
        -- Geometry
        bindInterleavedVbo ivbo
        drawArrays Triangles 0 6
        printError
        GLFW.swapBuffers

updateMatrixUniforms :: Matrix GLfloat -> Matrix GLfloat -> IO ()
updateMatrixUniforms projection modelview = do
    p <- getCurrentProgram
    -- Get uniform locations
    projectionLoc <- withCString "projection" $ \ptr ->
        glGetUniformLocation (programID p) ptr
    modelviewLoc <- withCString "modelview" $ \ptr ->
        glGetUniformLocation (programID p) ptr
    samplerLoc <- withCString "sampler" $ \ptr ->
        glGetUniformLocation (programID p) ptr
    printError

    -- Clear matrix uniforms.
    withArray (concat projection) $ \ptr ->
        glUniformMatrix4fv projectionLoc 1 1 ptr
    withArray (concat modelview) $ \ptr ->
        glUniformMatrix4fv modelviewLoc 1 1 ptr
    printError

    -- Clear the texture sampler.
    glUniform1i samplerLoc 0
    printError

updateUniforms :: [String]                                 -- ^ A list of uniform names.
               -> [GLint -> Ptr GLfloat -> IO ()] -- ^ A list of update functions.
               -> [[GLfloat]]                              -- ^ A list of uniform arrays.
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

