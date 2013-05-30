module Arbor.Graphics.Shaders where

import Arbor.Graphics.Util
import Graphics.Rendering.OpenGL
import Control.Monad                ( unless )

-- |Load a shader from a file.
loadShader :: (Shader s) => FilePath -> IO s
loadShader file = do
    -- Read in the shader source.
    src <- readFile file
    -- Generate a shader id.
    [shader] <- genObjectNames 1
    -- Set the source of the shader.
    shaderSource shader $= [src]
    -- Compile the shader and check for errors.
    compileShader shader
    printError
    ok      <- get (compileStatus shader)
    infoLog <- get (shaderInfoLog shader)
    unless (null infoLog)
           (mapM_ putStrLn
                  ["Shader info log for '" ++ file ++ "':", infoLog, ""])
    unless ok $ do
        deleteObjectNames [shader]
        ioError (userError "shader compilation failed")
    return shader

