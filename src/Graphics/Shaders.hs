{-# LANGUAGE TemplateHaskell #-}
module Graphics.Shaders where

import Geometry
import Graphics.Util
import Graphics.Rendering.OpenGL hiding ( Matrix )
--import Graphics.Rendering.OpenGL.GL.GLstring
import Graphics.Rendering.OpenGL.Raw.Core31
--import Graphics.Rendering.OpenGL.GL.Shaders.Program
import Control.Lens
import Control.Monad
import Data.Maybe

data ShaderDataType = Vec1
                    | Vec2
                    | Vec3
                    | Vec4
                    | Mat4
                    | Sam2D
                    deriving (Show, Eq)
data ShaderAttribute = Attribute ShaderDataType String deriving (Show, Eq)
data ShaderAttributeData a = AttributeData ShaderAttribute [a]
data ShaderUniform = Uniform ShaderDataType String deriving (Show, Eq)
data ShaderUniformUpdate = UpdateVec4 String (GLfloat,GLfloat,GLfloat,GLfloat)
                         | UpdateMat4 String (Matrix GLfloat)
                         | UpdateSampler String GLint
                         deriving (Show, Eq)

data ShaderProgram = ShaderProgram { _shaderName :: String
                                   , _shaderSrc  :: (FilePath,FilePath)
                                   , _attributes :: [ShaderAttribute]
                                   , _uniforms   :: [ShaderUniform]
                                   , _program    :: Maybe Program
                                   } deriving (Show, Eq)
makeLenses ''ShaderProgram

uniformName :: ShaderUniformUpdate -> String
uniformName (UpdateVec4 n _)    = n
uniformName (UpdateMat4 n _)    = n
uniformName (UpdateSampler n _) = n

uniformUpdate :: ShaderUniformUpdate -> RawUniformUpdateFn
uniformUpdate (UpdateVec4 _ _)    = (`glUniform4fv` 1)
uniformUpdate (UpdateMat4 _ _)    = \l -> glUniformMatrix4fv l 1 1
uniformUpdate (UpdateSampler _ i) = \l _ -> glUniform1i l i

uniformData :: ShaderUniformUpdate -> [GLfloat]
uniformData (UpdateVec4 _ (a,b,c,d)) = [a,b,c,d]
uniformData (UpdateMat4 _ mat)       = concat mat
uniformData (UpdateSampler _ _)      = []

uniformUpdateToUniform :: ShaderUniformUpdate -> ShaderUniform
uniformUpdateToUniform (UpdateVec4 n _)    = Uniform Vec4 n
uniformUpdateToUniform (UpdateMat4 n _)    = Uniform Mat4 n
uniformUpdateToUniform (UpdateSampler n _) = Uniform Sam2D n

uniformInShader :: ShaderUniformUpdate -> ShaderProgram -> Bool
uniformInShader ufrm sh = uniformUpdateToUniform ufrm `elem` sh ^. uniforms

updateUniform :: ShaderUniformUpdate -> ShaderProgram -> IO Bool
updateUniform ufrm sh =
    let name = uniformName ufrm
        up   = uniformUpdate ufrm
        arr  = uniformData ufrm
        mPgm = sh ^. program
    in if isJust mPgm
         then if ufrm `uniformInShader` sh
                then do currentProgram $= mPgm
                        updateUniformRaw name up arr
                        return True
                else do putStrLn $ "Shader does not contain " ++ show ufrm
                        return False
         else do putStrLn "Could not get program from shader."
                 return False


glGet :: GettableStateVar a -> IO a
glGet = Graphics.Rendering.OpenGL.get

--loadVboForShader :: (Storable a, Show a) => [ShaderAttributeData a] -> ShaderProgram -> IO (Maybe InterleavedVbo)
--loadVboForShader datas shdr =
--    if isNothing $ shdr ^. program
--    then return Nothing
--    else do (datas',comps,locs) <- foldM (\(ays,cs,ls) att@(AttributeData (Attribute _ std) data') -> do
--                    loc <- getAttribLocation att shdr
--                    return (ays++[data'],cs++[fromIntegral $ numComponents std],ls++[loc])) ([],[],[]) datas
--            fmap Just $ interleavedVbo datas' comps locs

--getAttribLocation :: ShaderAttributeData a -> ShaderProgram -> IO AttribLocation
--getAttribLocation (AttributeData (Attribute _ name) _) (ShaderProgram _ _ _ _ (Just (Program p))) =
--    fmap (AttribLocation . fromIntegral) $
--      withGLString name $
--         glGetAttribLocation p
--getAttribLocation _ _ = return $ AttribLocation $ fromIntegral (-1 :: Integer)

numComponents :: ShaderDataType -> Int
numComponents sdt
    | sdt == Vec1 = 1
    | sdt == Vec2 = 2
    | sdt == Vec3 = 3
    | sdt == Vec4 = 4
    | sdt == Mat4 = 16
    | otherwise = 1

loadShaderProgram :: ShaderProgram -> IO ShaderProgram
loadShaderProgram s =
   if isJust $ s ^. program
   then return s
   else do putStrLn $ "Loading shader program def " ++ show s
           v <- loadShader $ s ^. shaderSrc . _1 :: IO VertexShader
           f <- loadShader $ s ^. shaderSrc . _2 :: IO FragmentShader
           [p] <- genObjectNames 1
           attachedShaders p $= ([v],[f])
           foldM_ (\i (Attribute _ name) -> do
               attribLocation p name $= AttribLocation i
               return $ i+1) 0 (s ^. attributes)
           linkProgram p
           linked <- glGet $ linkStatus p
           unless linked $ do
               programLog <- glGet $ programInfoLog p
               putStrLn programLog
           currentProgram $= Just p
           validateProgram p
           printError
           return $ if not linked
                    then s
                    else program .~ Just p $ s

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


