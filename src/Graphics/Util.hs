module Graphics.Util where

import Graphics.Rendering.OpenGL
import Graphics.Rendering.OpenGL.Raw
import Graphics.Rendering.OpenGL.GL.Shaders.Program
import Foreign.Ptr              ( Ptr )
import Foreign.Marshal.Array    ( withArray )
import Foreign.C.String         ( withCString )
import System.IO                (hPutStrLn, stderr)

printError :: IO ()
printError = get errors >>= mapM_ (hPutStrLn stderr . ("GL: "++) . show)

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


