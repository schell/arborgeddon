module Graphics.Util where

import Graphics.Rendering.OpenGL
import Graphics.Rendering.OpenGL.Raw
import Graphics.Rendering.OpenGL.GL.Shaders.Program
import Foreign.Ptr              ( Ptr )
import Foreign.Marshal.Array    ( withArray )
import Foreign.C.String         ( withCString )
import System.IO                (hPutStrLn, stderr)

type RawUniformUpdateFn = GLint -> Ptr GLfloat -> IO ()

printError :: IO ()
printError = get errors >>= mapM_ (hPutStrLn stderr . ("GL: "++) . show)

updateUniformRaw :: String           -- ^ The uniform name.
               -> RawUniformUpdateFn -- ^ The update function.
               -> [GLfloat]          -- ^ the uniform array.
               -> IO ()
updateUniformRaw name update data' = do
    p   <- getCurrentProgram
    loc <- withCString name $ \ptr -> do
                l <- glGetUniformLocation (programID p) ptr
                printError
                return l
    withArray data' $ update loc
    printError

